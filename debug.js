'use strict'
/* eslint-env node, mocha */
console.time('thesearch')
let jsonfilter = require('./jsonfilter')
let util = require('./util')
let randomaccess = require('diskbased_json_array')

let database = require('./database')
let searchDb = require('./searchDb')

return searchDb.searchDb('mochaTest', {search: {
    term:'majestätischer',
    path:'meanings.ger[]',
    levenshtein_distance:1,
    firstCharExactMatch:true
}})