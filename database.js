'use strict'
console.time('thesearch')
let jsonfilter = require('./jsonfilter')
let randomaccess = require('diskbased_json_array')
let createindex = require('./createindex')

function ensureFolderExists(dbfolder){
    var mkdirp = require('mkdirp')
    mkdirp.sync(dbfolder)
}

function createDatabase(data, dbfolder, indices, filterSchema){
    
    let parentDir = process.cwd()
    ensureFolderExists(dbfolder)
    process.chdir(parentDir+'/'+dbfolder)

    if(filterSchema)
        data = jsonfilter.filterWithSchema(data, filterSchema)
    randomaccess.writeArray('json_data', data)
    return createindex.createIndices(data, indices)
    .then(()=> {
        process.chdir(parentDir)
    })
    .catch(err => {
        console.log(err)
        throw err
    })
}

function createDatabaseFromFile(filename, dbfolder, indices, filterSchema){
    let data = JSON.parse(require('fs').readFileSync(filename))
    return createDatabase(data, dbfolder, indices, filterSchema)
}

let service = {
    createDatabase: createDatabase,
    createDatabaseFromFile: createDatabaseFromFile
}
module.exports = service