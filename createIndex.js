'use strict'
var fs = require('fs')
var util = require('./util')

function binarySearch(arr, find) {
    var low = 0, high = arr.length - 1,i
    while (low <= high) {
        i = Math.floor((low + high) / 2)
        // comparison = comparator(arr[i], find);
        if (arr[i] < find) { low = i + 1; continue }
        if (arr[i] > find) { high = i - 1; continue }
        return i
    }
    return null
}

function getValueID(data, value){
    return binarySearch(data, value)
}

function isInStopWords(term, options){
    return options.stopwords.indexOf(term) >= 0
}

function getAllterms(data, path, options){
    options = options || {}
    let terms = {}

    forEachElementInPath(data, path, (value) => {
        let normalizedText = util.normalizeText(value)
        if(!isInStopWords(normalizedText, options)) terms[normalizedText] = true
        if (options.tokenize) 
            forEachToken(normalizedText, token => {if(!isInStopWords(normalizedText, options)) terms[token] = true})
    })
    return Object.keys(terms).sort()
}

// let terms = getAllterms(data, "kanji.text".split("."))
// console.log(terms[1000]);


function forEachElementInPath(data, path, cb) {
    path = util.removeArrayMarker(path)
    let paths = path.split('.')
    let valueId = 0
    let lastPath = paths[paths.length - 1]

    for (let mainId = 0; mainId < data.length; mainId++) {  
        let entry = data[mainId]
        let currentEl = entry
        // let mainId = entry.ent_seq
        for (let i = 0; i < paths.length; i++) {
            let comp = paths[i]
            if (currentEl[comp] === undefined) break
            currentEl = currentEl[comp]

            if(Array.isArray(currentEl)){
                if (lastPath == comp){
                    currentEl.forEach(el => {
                        cb(el, mainId, valueId)
                        valueId++
                    })
                }else{
                    comp = paths[++i] // move to next level
                    for(let subarrEl of currentEl){
                        // if (lastPath == comp) 
                        // if (subarrEl[comp] === undefined) continue
                        if (lastPath == comp && subarrEl[comp]){
                            cb(subarrEl[comp], mainId, valueId)
                        }else{
                            // throw new Error('level 3 not supported')
                        }
                        valueId++
                        
                    }
                }
            }else{
                if (lastPath == comp){
                    cb(currentEl, mainId, valueId)
                }
            }

        }

    }
}


function sortFirstColumn(a, b) {
    if (a[0] === b[0])
        return 0
    else
        return (a[0] < b[0]) ? -1 : 1
}

function writeFileSync(file, data){
    fs.writeFileSync(file, data)
}

function forEachToken(normalizedText, cb){
    normalizedText = normalizedText.replace(/[-,.'"]/g, ' ') // remove ' " {}
    normalizedText = normalizedText.replace(/\s\s+/g, ' ') // replace tabs, newlines, double spaces with single spaces
    normalizedText.split(' ').forEach(term => {
        cb(term)
    })

    // const regex = /(\w*)/g
    // let m

    // while ((m = regex.exec(normalizedText)) !== null) {
    //     // This is necessary to avoid infinite loops with zero-width matches
    //     if (m.index === regex.lastIndex) {
    //         regex.lastIndex++
    //     }
        
    //     // The result can be accessed through the `m`-variable.
    //     m.forEach((match) => {
    //         if (match.length >= 2) cb(match)
    //     })
    // }
}

// forEachToken(util.normalizeText("(f) spielen (ein Musikinstrument, ein Musikstück)"), console.log.bind(console))

// function isLastPath(paths, path){
//     return paths.indexOf(path) === (paths.length -1)
// }

function createFulltextIndex(data, path, options){
    // let subfolder = options.subfolder || ''
    return new Promise((resolve) => {

        options = options || {}
        options.stopwords = options.stopwords || []
        let allTerms = getAllterms(data, path, options)

        // let tuples = []
        let tokens = []

        let paths = util.getStepsToAnchor(path)
        console.log("StepsToAnchor")
        console.log(paths)

        paths.forEach((pathToAnchor, index) => {
            let level = util.getLevel(pathToAnchor)
            let tuples = []

            let isLast = index === (paths.length -1)

            forEachElementInPath(data, pathToAnchor, function (value, mainId, subObjId) {
                if (isLast){
                    let normalizedText = util.normalizeText(value)
                    if(isInStopWords(normalizedText, options)) return

                    let valId = getValueID(allTerms, normalizedText)
                    // tuples.push((subObjId !== undefined) ? [valId, mainId, subObjId] : [valId, mainId])
                    tuples.push([valId , arguments[level + 1]])
                    if (options.tokenize && normalizedText.split(' ').length > 1) 
                        forEachToken(normalizedText, token => {if(!isInStopWords(normalizedText, options)) tokens.push([getValueID(allTerms, token), valId])})

                }else{
                    tuples.push([arguments[level + 1], arguments[1]])
                }
            })
            tuples.sort(sortFirstColumn)
            let pathName = util.getPathName(pathToAnchor, isLast) // last path is for the textindex
            writeFileSync(pathName+'.valueIdToParent.valIds', new Buffer(new Uint32Array(tuples.map(tuple => tuple[0])).buffer))
            writeFileSync(pathName+'.valueIdToParent.mainIds', new Buffer(new Uint32Array(tuples.map(tuple => tuple[1])).buffer))
        })

        // writeFileSync(path+'.valIds', new Buffer(new Uint32Array(tuples.map(tuple => tuple[0])).buffer))
        // writeFileSync(path+'.mainIds', new Buffer(new Uint32Array(tuples.map(tuple => tuple[1])).buffer))

        if (tokens.length > 0) {
            tokens.sort(sortFirstColumn)
            writeFileSync(path+'.tokens.tokenValIds', new Buffer(new Uint32Array(tokens.map(tuple => tuple[0])).buffer))
            writeFileSync(path+'.tokens.parentValId', new Buffer(new Uint32Array(tokens.map(tuple => tuple[1])).buffer))
        }
        // writeFileSync(path, new Buffer(JSON.stringify(allTerms)))
        writeFileSync(path, allTerms.join('\n'))
        writeFileSync(path+'.length', new Buffer(new Uint32Array(allTerms.map(term => term.length)).buffer))
        creatCharOffsets(path, resolve)
    })
    .catch(err => {
        throw new Error('Error while creating index: ' + path + ' : '+err.toString())
    })

}

function creatCharOffsets(path, resolve){

    const readline = require('readline')
    let stream = fs.createReadStream(path)
    const rl = readline.createInterface({ input: stream })

    let offsets = []

    let currentSingleChar, currentSecondChar
    let byteOffset = 0, lineNum = 0, currentChar, currentTwoChar
    rl.on('line', (line) => {
        let firstCharOfLine = line.charAt(0)
        let firstTwoCharOfLine = line.charAt(0) + line.charAt(1)
        if(currentChar != firstCharOfLine){
            currentChar = firstCharOfLine
            if(currentSingleChar) currentSingleChar.byteOffsetEnd = byteOffset
            currentSingleChar = {char: currentChar, byteOffsetStart:byteOffset, lineOffset:lineNum}
            offsets.push(currentSingleChar)
            console.log(`${currentChar} ${byteOffset} ${lineNum}`)
        }
        if(currentTwoChar != firstTwoCharOfLine){
            currentTwoChar = firstTwoCharOfLine
            if(currentSecondChar) currentSecondChar.byteOffsetEnd = byteOffset
            currentSecondChar = {char: currentTwoChar, byteOffsetStart:byteOffset, lineOffset:lineNum}
            offsets.push(currentSecondChar)
            console.log(`${currentTwoChar} ${byteOffset} ${lineNum}`)
        }
        byteOffset+= Buffer.byteLength(line, 'utf8') + 1 // linebreak = 1
        lineNum++
    }).on('close', () => {
        if(currentSingleChar) currentSingleChar.byteOffsetEnd = byteOffset
        if(currentSecondChar) currentSecondChar.byteOffsetEnd = byteOffset
        writeFileSync(path+'.charOffsets.chars', JSON.stringify(offsets.map(offset=>offset.char)))
        writeFileSync(path+'.charOffsets.byteOffsetsStart',     new Buffer(new Uint32Array(offsets.map(offset=>offset.byteOffsetStart)).buffer))
        writeFileSync(path+'.charOffsets.byteOffsetsEnd',  new Buffer(new Uint32Array(offsets.map(offset=>offset.byteOffsetEnd)).buffer))
        writeFileSync(path+'.charOffsets.lineOffset',  new Buffer(new Uint32Array(offsets.map(offset=>offset.lineOffset)).buffer))
        resolve()
    })
}



function createBoostIndex(data, path, options){
    options = options || {}
    // path = util.removeArrayMarker(path)

    // let level = util.getLevel(pathToAnchor)
    // if (level !== 0) throw new Error("Only level 0 boosts supported")

    let tuples = []
    let level = util.getLevel(path)
    forEachElementInPath(data, path,function (value, mainId, subObjId)  {
        if (options.type == 'int') {
            tuples.push([arguments[level + 1], value])
            // tuples.push([mainId, value])
        }else{
            throw new Error('only type int supported for boost')
        }
    })
    tuples.sort(sortFirstColumn)

    writeFileSync(path+'.boost.subObjId', new Buffer(new Uint32Array(tuples.map(tuple => tuple[0])).buffer))
    // writeFileSync(path+'.boost.mainId', new Buffer(new Uint32Array(tuples.map(tuple => tuple[0])).buffer))
    writeFileSync(path+'.boost.value', new Buffer(new Uint32Array(tuples.map(tuple => tuple[1])).buffer))
}

function createIndices(data, indices){

    return Promise.all(indices.map(index => {
        if (index.fulltext) {
            return createFulltextIndex(data, index.fulltext, index.options)
        }else if(index.boost){
            return createBoostIndex(data, index.boost, index.options)
        }else{
            throw new Error('Choose boost or fulltext')
        }
    }))

}


var service = {}
service.createFulltextIndex = createFulltextIndex
service.createBoostIndex = createBoostIndex
service.createIndices = createIndices
module.exports = service
