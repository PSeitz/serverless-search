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
    return require('./loadUint32')(path)
}

class CharOffset{
    constructor(path){
        this.chars = JSON.parse(fs.readFileSync(path+'.charOffsets.chars'))
        this.byteOffsets = getIndex(path+'.charOffsets.byteOffsets')
        this.lineOffsets = getIndex(path+'.charOffsets.lineOffset')
    }
    getClosestOffset(linePos){
        let pos = lowerBoundSearch(this.lineOffsets, linePos)
        return this.getOffsetInfo(pos)
    }
    getCharOffsetInfo(char){
        return this.getOffsetInfo(binarySearch(this.chars, char) )
    }
    getOffsetInfo(pos){
        let byteRange = {start: this.byteOffsets[pos], end:this.byteOffsets[pos+1]-1} // -1 For the linebreak
        return {byteRange: byteRange, lineOffset: this.lineOffsets[pos]}
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
    // get subObjIds(){ return this.store.values2 }
    getParentValId(key){
        return this.store.getValue(key)
    }
    getParentValIds(key){
        return this.store.getValues(key)
    }
    // getSubObjId(key){
    //     return this.store.getValue2(key)
    // }
    // getTextForValueId(index){
    //     return getLine(this.path, this.parentValIds[index])
    // }
    getTextForValueId(index){
        return getLine(this.path, index)
    }
}


function removeArrayMarker(path){
    return path.split('.')
        .map(el => (el.endsWith('[]')? el.substr(0, el.length-2):el ))
        .join('.')
}

function getAllParentValIds(valueIdHits, scoreHits, valIdsIndex){

    let valueIdDocids = []
    let valueIdDocidScores = []
    valueIdHits.forEach((hit, index) => {
        let rows = binarySearchAll(valIdsIndex, hit)
        valueIdDocids = valueIdDocids.concat(rows)
        valueIdDocidScores = valueIdDocidScores.concat(rows.map(() => scoreHits[index]))
    }) // For each hit in the fulltextindex, find all rows in the materialized index
    return {
        valueIdDocids:valueIdDocids,
        valueIdDocidScores: valueIdDocidScores
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
        console.log("START at Line: " + charOffset.lineOffset)
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
    return 2/(levenshtein.get(term1, term2) + 1 )
}

function addBoost(request, hits){
    let boostPath = request.boost.path
    let boostkvStore = new IndexKeyValueStore(boostPath+'.boost.subObjId', boostPath+'.boost.value')
    for (let valueId in hits) {
        let score = request.boost.fun(boostkvStore.getValue(valueId) + (request.boost.param || 0) )
        // console.log("THE SCORE")
        // console.log(score)
        hits[valueId] += score
    }
}


function addTokenResults(hits, path, term){
    let hasTokens = fs.existsSync(path+'.tokens.tokenValIds')
    if (!hasTokens) return Promise.resolve(hits)

    let tokenKVData = new TokensIndexKeyValueStore(path)
    let tokenParentids = []

    for (let valueId in hits) {
        let parentIdsForToken = tokenKVData.getParentValIds(parseInt(valueId))
        if(parentIdsForToken.length > 0){
            delete hits[valueId]
            tokenParentids = tokenParentids.concat(parentIdsForToken)
        }
    }

    return Promise.all(tokenParentids.map(tokenParentvalId => {
        return tokenKVData.getTextForValueId(tokenParentvalId)
        .then(parentString => {
            let score = getDefaultScore(parentString, term)
            if(hits[tokenParentvalId]) hits[tokenParentvalId].score += score
            else hits[tokenParentvalId] = {score:score}
        })
    }))
    .then(() => hits)

}


/* Returns then value ids and scores*/
function getHitsInField(path, options, term){
    let hits = {} // id:score
    let lineoptions = {path:path}
    let checks = []

    //exact when nothing else set
    if (options.exact === undefined && options.levenshtein_distance === undefined && options.startsWith === undefined && options.customCompare === undefined)
        options.exact = true

    if (options.exact !== undefined) checks.push(line => line == term)
    if (options.levenshtein_distance !== undefined) checks.push(line => levenshtein.get(line, term) <= options.levenshtein_distance)
    if (options.startsWith !== undefined) checks.push(line => line.startsWith(term))
    if (options.customCompare !== undefined) checks.push(line => options.customCompare(line))

    //Check limit search on starting char
    if (options.firstCharExactMatch || options.exact || options.levenshtein_distance === 0 || options.startsWith !== undefined) lineoptions.char = term.charAt(0)

    return getTextLines(lineoptions, (line, linePos) => {
        // console.log("Check: "+line + " linePos:"+linePos)
        if (checks.every(check => check(line))){
            console.log("Hit: "+line + " linePos:"+linePos)

            let score = options.customScore ? options.customScore(line, term) : getDefaultScore(line, term)
            if(hits[linePos]) hits[linePos].score += score
            else hits[linePos] = {score:score}

            if (options.includeValue) hits[linePos].value = line
        }
    }).then(() => {
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
            r.push(a);
        }
        return r;
    }, []);
}

function search(request){
    return searchUnrolled(request)
        .then(hitsToArray)
        .then(sortByScore)
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
    let term = request.search.term.toLowerCase()
    let options = request.search

    // let request = {
    //     OR: [
    //         {
    //             search: {
    //                 term:'maje',
    //                 path:'meanings.ger[]',
    //                 levenshtein_distance:1
    //             }
    //         },
    //         {
    //             search: {
    //                 term:'maje',
    //                 path:'meanings.eng[]',
    //                 levenshtein_distance:1
    //             }
    //         },

    //     ]
    // }

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

    // let origPath = path
    // path = removeArrayMarker(path)
    console.time('SearchTime Netto')

    return getHitsInField(path, options, term)
    .then(res => addTokenResults(res, path, term))
    .then(hits => {

        console.log("hits")
        console.log(hits)

        let nextLevelHits = {}

        let paths = util.getStepsToAnchor(path)

        for(let i=paths.length ; i -- > 0;){
            let path = paths[i]
            let isLast = i === (paths.length -1)
            let pathName = util.getPathName(path, isLast) // last path is for the textindex

            if (request.boost && request.boost.path && request.boost.path.indexOf(pathName) >= 0) { // TODO move towards path
                addBoost(request, hits)
            }

            let kvStore = new IndexKeyValueStore(pathName+'.valueIdToParent.valIds', pathName+'.valueIdToParent.mainIds')                
            for (let valueId in hits) {
                let score = hits[valueId].score
                let values = kvStore.getValues(parseInt(valueId, 10))
                values.forEach(parentValId => {
                    if(nextLevelHits[parentValId]) nextLevelHits[parentValId].score += score
                    else nextLevelHits[parentValId] = {score:score}
                })
            }
            hits = nextLevelHits
            nextLevelHits = {}
        }

        return hits

        // let mainWithScore = sortByScore(hitsToArray(hits))

        // console.log(mainWithScore)
        // console.timeEnd('SearchTime Netto')
        // return mainWithScore
    })


}


let service = {}
service.getHitsInField = getHitsInField
service.search = search
service.hitsToArray = hitsToArray
service.sortByScore = sortByScore
service.suggest = suggest
module.exports = service
