'use strict'

let jsonfilter = require('./jsonfilter')

let database = require('./database')

var schema = {
    'pos': true,
    'misc': true,
    'kanji': [
        {
            'text': true,
            'commonness': true
            // 'num_occurences': true,
            // 'readings': true
        }
    ],
    'kana': [
        {
            'text': true,
            'romaji': true,
            'commonness': true
            // 'num_occurences': true
        }
    ],
    'meanings':
    {
        'eng': true,
        'ger': true
    }
    ,
    'ent_seq': true
}

return database.createDatabaseFromFile('jmdict.json', 'jmdict',  [
    { fulltext:'entseq' },
    { fulltext:'kanji[].text' }, 
    { fulltext:'kana[].romaji' }, 
    // { fulltext:'kana[].text' }, 
    // { fulltext:'kanji[].text' }, 
    { fulltext:'meanings.ger[]', options:{tokenize:true} }, 
    { fulltext:'meanings.eng[]', options:{tokenize:true} } 
    // { boost:'kanji[].commonness' , options:{type:'int'}}, 
    // { boost:'kana[].commonness', options:{type:'int'} }
], schema)
.catch(console.log)