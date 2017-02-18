'use strict'
let fs = require('fs')

let service = {}

function removeArrayMarker(path){
    return path.split('.')
        .map(el => (el.endsWith('[]')? el.substr(0, el.length-2):el ))
        .join('.')
}

function normalizeText(text){
    // text = text.replace(/ *\([^)]*\) */g, ' ') // remove everything in braces
    text = text.replace(/ *\([fmn\d)]*\) */g, ' ') // remove (f)(n)(m)(1)...(9)
    text = text.replace(/[\(\)]/g, ' ') // remove braces
    text = text.replace(/[{}'"“]/g, '') // remove ' " {}
    text = text.replace(/\s\s+/g, ' ') // replace tabs, newlines, double spaces with single spaces
    text = text.replace(/[,.…]/g, '') // remove , .
    text = text.replace(/[;・’-]/g, '') // remove ;・’-
    text = text.toLowerCase()
    return text.trim()
}

function getStepsToAnchor(path){
    let paths = []
    let current = []
    let parts = path.split('.')
    parts.forEach(part => {
        current.push(part)
        if (part.endsWith('[]'))
            paths.push(current.join('.'))
    })
    paths.push(path) // add complete path
    return paths
}

function getLevel(path){
    return (path.match(/\[\]/g) || []).length
}

function getPathName(pathToAnchor, isTextIndexPart){
    return pathToAnchor + (isTextIndexPart?'.textindex':'')
}

service.normalizeText = normalizeText
service.removeArrayMarker = removeArrayMarker
service.getStepsToAnchor = getStepsToAnchor
service.getLevel = getLevel
service.getPathName = getPathName

module.exports = service