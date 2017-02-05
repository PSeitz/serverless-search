'use strict'
let randomaccess = require('diskbased_json_array')
let searchindex = require('./searchindex')


function searchDb(dbfolder, request){

    let parentDir = process.cwd()
    if (!process.cwd().endsWith(dbfolder))
        process.chdir(process.cwd()+'/'+dbfolder)

    return searchindex.search(request)
    .then(mainWithScore => {
        let loader = new randomaccess.Loader('json_data')
        return loader.getDocs(mainWithScore.map(el => el.id))
        .then(docs => {
            docs.forEach((doc, index) => doc.score = mainWithScore[index].score)
            return docs
        })
    }).then(data => {
        process.chdir(parentDir)
        return data
    }).catch(err => {
        process.chdir(parentDir)
        throw err
    })
    
}

function suggestDb(dbfolder, path, term){

    let parentDir = process.cwd()
    if (!process.cwd().endsWith(dbfolder))
        process.chdir(process.cwd()+'/'+dbfolder)

    return searchindex.suggest(path, term)
    .then(data => {
        process.chdir(parentDir)
        return data
    }).catch(err => {
        process.chdir(parentDir)
        throw err
    })
    
}

let service = {
    searchDb: searchDb
}
module.exports = service