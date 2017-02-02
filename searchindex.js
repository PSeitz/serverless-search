'use strict'
let fs = require('fs')
let levenshtein = require('fast-levenshtein')
var util = require('./util')

function binarySearchAll(arr, find) {
    let low = 0, high = arr.length - 1,i
    while (low <= high) {
        i = Math.floor((low + high) / 2)
        // comparison = comparator(arr[i], find);
        if (arr[i] < find) { low = i + 1  }
        else { high = i - 1  }
        // low = i + 1;
        // return i
    }
    if (arr[i] !== find && arr[i+1 !== find]) return null
    
    if (arr[i] !== find) i++

    let allPos = []
    while(arr[i] === find){
        allPos.push(i)
        i++
    }
    return allPos
}

function binarySearch(arr, find) {
    let low = 0, high = arr.length - 1,i
    while (low <= high) {
        i = Math.floor((low + high) / 2)
    // comparison = comparator(arr[i], find);
        if (arr[i] < find) { low = i + 1; continue }
        if (arr[i] > find) { high = i - 1; continue }
        return i
    }
    return null
}

function lowerBoundSearch(arr, find) {
    let high = arr.length-1
    let low = 0
    var i=low-1
    while(low<=high){
        var m=(low+high)>>>1
        if(arr[m]<=find){i=m;low=m+1}
        else{high=m-1}
    }
    return i
}


function getIndex(path){
    return require('./loadUInt32')(path)
}

class CharOffset{
    constructor(path){
        this.chars = JSON.parse(fs.readFileSync(path+'.charOffsets.chars'))
        this.byteOffsetsStart = getIndex(path+'.charOffsets.byteOffsetsStart')
        this.byteOffsetsEnd = getIndex(path+'.charOffsets.byteOffsetsEnd')
        this.lineOffsets = getIndex(path+'.charOffsets.lineOffset')
    }
    getClosestOffset(linePos){
        let index = lowerBoundSearch(this.lineOffsets, linePos)
        return this.getOffsetInfo(index)
    }
    getCharOffsetInfo(char){
        let charIndex = binarySearch(this.chars, char)
        return this.getOffsetInfo(charIndex)
    }
    getOffsetInfo(index){
        let byteRange = {start: this.byteOffsetsStart[index], end:this.byteOffsetsEnd[index]-1} // -1 For the linebreak
        return {byteRange: byteRange, lineOffset: this.lineOffsets[index]}
    }
}

class IndexKeyValueStore{
    constructor(key, value1, value2){
        this.keys = typeof key === 'string' ? getIndex(key) : key
        this.values = typeof value1 === 'string' ? getIndex(value1) : value1
        if(value2) this.values2 = typeof value2 === 'string' ? getIndex(value2) : value2
    }
    getValue(key){
        let pos = binarySearch(this.keys, key)
        return this.values[pos]
    }
    getValues(key){
        let rows = binarySearchAll(this.keys, key)
        return rows.map(row => this.values[row])
    }
    getValue2(key){
        let pos = binarySearch(this.keys, key)
        return this.values2[pos]
    }
}

class TokensIndexKeyValueStore{
    constructor(path){
        this.path = path
        this.store = new IndexKeyValueStore(path+'.tokens.tokenValIds', path+'.tokens.parentValId')
    }
    get keys() { return this.store.keys }
    get parentValIds(){ return this.store.values }
    getParentValId(key){
        return this.store.getValue(key)
    }
    getParentValIds(key){
        return this.store.getValues(key)
    }
    getTextForValueId(index){
        return getLine(this.path, index)
    }
}

let charOffsetCache = {}

function getCreateCharOffsets(path) {
    charOffsetCache[path] = charOffsetCache[path] || new CharOffset(path)
    return charOffsetCache[path]
}

function getTextLines(options, onLine){ //options: path, char
    let charOffset = {lineOffset:0}
    if(options.char){
        charOffset = getCreateCharOffsets(options.path).getCharOffsetInfo(options.char)
        console.log(options.char + " START at Line: " + charOffset.lineOffset)
    }
    if (options.linePos) {
        charOffset = getCreateCharOffsets(options.path).getClosestOffset(options.linePos)
    }
    return new Promise(resolve => {
        const readline = require('readline')
        let stream = fs.createReadStream(options.path, charOffset.byteRange)
        const rl = readline.createInterface({ input: stream})
        rl.on('line', line => {
            onLine(line, charOffset.lineOffset)
            charOffset.lineOffset++
        })
        rl.on('close', resolve)
    })
}

function getLine(path, linePos){ //options: path, char
    return new Promise(resolve => {
        getTextLines({path:path, linePos:linePos}, (lineText, currentLinePos) => {
            if (currentLinePos == linePos) {
                // console.log(lineText)
                resolve(lineText)
            }
        })
    })
}

function getDefaultScore(term1, term2){
    return 2/(levenshtein.get(term1, term2) + 0.2 )
}

function addBoost(boost, hits){
    let boostPath = boost.path
    let boostkvStore = new IndexKeyValueStore(boostPath+'.boost.subObjId', boostPath+'.boost.value')
    for (let valueId in hits) {
        let score = boost.fun(boostkvStore.getValue(valueId) + (boost.param || 0) )
        // console.log("THE SCORE")
        // console.log(score)
        if (score) hits[valueId].score += score
        
    }
}


function addTokenResults(hits, path, term){
    let hasTokens = fs.existsSync(path+'.tokens.tokenValIds')
    if (!hasTokens) return Promise.resolve(hits)

    var hrstart = process.hrtime()
    let tokenKVData = new TokensIndexKeyValueStore(path)
    let valueLengths = getIndex(path+'.length')

    for (let valueId in hits) {
        let parentIdsForToken = tokenKVData.getParentValIds(parseInt(valueId))
        if(parentIdsForToken.length > 0){
            let tokenScore = hits[valueId].score
            parentIdsForToken.forEach(tokenParentvalId => {
                let parentTextLength = valueLengths[tokenParentvalId]
                let tokenTextLength = valueLengths[valueId]
                let adjustedScore = tokenScore * tokenTextLength / parentTextLength
                if(hits[tokenParentvalId]) hits[tokenParentvalId].score += adjustedScore
                else hits[tokenParentvalId] = {score:adjustedScore}
            })
        }
    }
    console.info("addTokenResultsTime: %dms",  process.hrtime(hrstart)[1]/1000000)
    return hits
}


/* Returns then value ids and scores*/
function getHitsInField(path, options, term){
    let hits = {} // id:score
    let lineoptions = {path:path}

    options.checks = options.checks || []

    if (options.operator && ['every', 'some'].indexOf(options.operator) === -1) throw new Error('options.operator must be "some" or "every"')

    let checksmethod = options.checks[options.operator || 'every'].bind(options.checks) // ''every' or 'some'

    //exact when nothing else set
    if (options.exact === undefined && options.levenshtein_distance === undefined && options.startsWith === undefined && options.customCompare === undefined)
        options.exact = true

    if (options.exact !== undefined) options.checks.push(line => line == term)
    if (options.levenshtein_distance !== undefined) options.checks.push(line => levenshtein.get(line, term) <= options.levenshtein_distance)
    if (options.startsWith !== undefined) options.checks.push(line => line.startsWith(term))
    if (options.customCompare !== undefined) options.checks.push(line => options.customCompare(line))

    //Check limit search on starting char
    if (options.firstCharExactMatch)  lineoptions.char = term.charAt(0)

    if (options.exact || options.levenshtein_distance === 0 || options.startsWith !== undefined)
        lineoptions.char = term.charAt(0) + term.charAt(1)

    var hrstart = process.hrtime()
    return getTextLines(lineoptions, (line, linePos) => {
        // console.log("Check: "+line + " linePos:"+linePos)
        if (checksmethod(check => check(line))){
            
            let score = options.customScore ? options.customScore(line, term) : getDefaultScore(line, term)
            if(hits[linePos]) hits[linePos].score += score
            else hits[linePos] = {score:score}

            // console.log("Hit: "+line + " linePos:"+linePos + " score:"+score)
            if (options.includeValue) hits[linePos].value = line
        }
    }).then(() => {
        console.info("getHitsInFieldTime: %dms",  process.hrtime(hrstart)[1]/1000000)
        return hits
    })
}


function hitsToArray(hits){
    let mainWithScore = []
    for (let valueId in hits) {
        hits[valueId].id = parseInt(valueId, 10)
        mainWithScore.push(hits[valueId])
    }
    return mainWithScore
}

function sortByScore(hits) {
    return hits.sort(function(a, b) {
        return ((a.score > b.score) ? -1 : ((a.score == b.score) ? 0 : 1))
    })
}

function suggest(path, term){
    return getHitsInField(path, {startsWith:true, includeValue:true}, term)
    .then(hitsToArray)
    .then(sortByScore)
}

function intersection(o1, o2) {
    return Object.keys(o1).concat(Object.keys(o2)).sort().reduce(function (r, a, i, aa) {
        if (i && aa[i - 1] === a) {
            r.push(a)
        }
        return r
    }, [])
}

function search(request){
    request.skip = request.skip || 0
    request.top = request.top || 10

    return searchUnrolled(request)
        .then(hitsToArray)
        .then(sortByScore)
        .then(res => res.slice(request.skip, request.top))
}

function searchUnrolled(request){
    if (request.OR) {
        return Promise.all(request.OR.map(req => searchUnrolled(req)))
        .then(results => results.reduce((p, c) => Object.assign(p, c)))
    }else if(request.AND){
        return Promise.all(request.AND.map(req => searchUnrolled(req)))
        .then(results => results
            .reduce((p, c) => intersection(p, c)
            .map(commonKey => ((p[commonKey].score > c[commonKey].score) ? p[commonKey] : c[commonKey]))))
    }else{
        return searchRaw(request)
    }
}

function searchRaw(request){

    let path = request.search.path
    let term = util.normalizeText(request.search.term)
    let options = request.search

    if (request.boost && !Array.isArray(request.boost)) request.boost = [request.boost]

    //     let request = {
    //     search: {
    //         term:'我慢汁',
    //         path:'kanji[].text',
    //         levenshtein_distance:1
    //     },
    //     boost: {
    //         attr:'kanji[].commonness',
    //         fun:'log'
    //     }
    // }

    // console.time('SearchTime Netto')
    var hrstart = process.hrtime()

    let boostHits = (boost, pathName) => boost.path && boost.path.indexOf(pathName) >= 0

    let checkApplyBoost = function(boost, pathName, hits){
        if (boostHits(boost, pathName)) { // TODO move towards path
            addBoost(boost, hits)
            return true
        }
        return  false
    }    

    return getHitsInField(path, options, term)
    .then(res => addTokenResults(res, path, term))
    .then(hits => {

        // console.log("hits")
        // console.log(hits)
        let nextLevelHits = {}

        let paths = util.getStepsToAnchor(path)

        for(let i=paths.length ; i -- > 0;){
            let path = paths[i]
            let isLast = i === (paths.length -1)
            let pathName = util.getPathName(path, isLast) // last path is for the textindex

            if (request.boost) { // TODO move towards path
                request.boost = request.boost.filter(boost => !checkApplyBoost(boost, pathName, hits))
            }

            let kvStore = new IndexKeyValueStore(pathName+'.valueIdToParent.valIds', pathName+'.valueIdToParent.mainIds')                
            for (let valueId in hits) {
                let score = hits[valueId].score
                let values = kvStore.getValues(parseInt(valueId, 10))
                values.forEach(parentValId => {
                    if(nextLevelHits[parentValId]) nextLevelHits[parentValId].score = Math.max(score, nextLevelHits[parentValId].score) + 0.1
                    else nextLevelHits[parentValId] = {score:score}
                })
            }
            hits = nextLevelHits
            nextLevelHits = {}
        }

        // if (request.boost && request.boost.path ) { // TODO move towards path
        //     addBoost(request, hits)
        //     delete request.boost
        // }
        if (request.boost) { // TODO move towards path
            request.boost = request.boost.filter(boost => !checkApplyBoost(boost, '', hits))
        }

        // console.log("hits")
        // console.log(hits)
        console.info("SearchTime Netto: %dms",  process.hrtime(hrstart)[1]/1000000)

        return hits
    })


}


let service = {}
service.getHitsInField = getHitsInField
service.search = search
service.hitsToArray = hitsToArray
service.sortByScore = sortByScore
service.suggest = suggest
module.exports = service
