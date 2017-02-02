'use strict'
/* eslint-env node, mocha */
console.time('thesearch')
let jsonfilter = require('./jsonfilter')
let util = require('./util')
let chai = require('chai')
let chaiAsPromised = require('chai-as-promised')
chai.use(chaiAsPromised)

let expect = require('chai').expect
let should = require('chai').should()

var fs = require('fs')
var deleteFolderRecursive = function(path) {
    if( fs.existsSync(path) ) {
        fs.readdirSync(path).forEach(function(file,index){
            var curPath = path + "/" + file
            if(fs.lstatSync(curPath).isDirectory()) { // recurse
                deleteFolderRecursive(curPath)
            } else { // delete file
                fs.unlinkSync(curPath)
            }
        })
        fs.rmdirSync(path)
    }
}


let database = require('./database')

let data = [
    {                                           // anchor id 0
        "commonness": 20,
        "kanji": [
            { "text": "偉容", "commonness": 0}, // kanji id 0
            { "text": "威容","commonness": 5}   // kanji id 1
        ],
        "kana": [
            {
                "text": "いよう",
                "romaji": "Iyou",
                "commonness": 5,
            }
        ],
        "meanings": {   // meanings id 0
            "eng" : ["dignity", "majestic appearance", "will"],
            "ger": ["majestätischer Anblick (m)", "majestätisches Aussehen (n)", "Majestät (f)"] // meanings.ger id 0, 1, 2 .. 
        },
        "ent_seq": "1587680"
    },
    {                                           // anchor id 1
        "commonness": 20,
        "kanji": [
            { "text": "意欲", "commonness": 40}, // kanji id 2
            { "text": "意慾", "commonness": 0}   // kanji id 3
        ],
        "kana": [
            {
                "text": "いよく",
                "romaji": "Iyoku",
                "commonness": 40,
            }
        ],
        "meanings": { // meanings id 1
            "eng" : ["will", "desire", "urge", "having a long torso"],
            "ger": ["Wollen (n)", "Wille (m)", "Begeisterung (f)"] // meanings.ger id .. 5, 6 7 
        },
        "ent_seq": "1587690"
    },
    {          
        "commonness": 500,                                 // anchor id 2
        "kanji": [
            { "text": "意慾", "commonness": 20}   // kanji id 4
        ],
        "field1" : {text:"awesome", rank:1},
        "kana": [
            {
                "text": "いよく",
            }
        ],
        "meanings": { // meanings id 2
            "eng" : ["test1"],
            "ger": ["der test"] // meanings.ger id ..
        },
        "ent_seq": "1587700"
    },
    {                                          
        "commonness": 551,                       
        "kanji": [                               
            {                                      
                "text": "何の",                        
                "commonness": 526                    
            }                                      
        ],          
        "field1" : {text:"awesome"},                             
        "kana": [                                
            {                                      
                "text": "どの",                        
                "romaji": "Dono",                    
                "commonness": 25                     
            }                                      
        ],                                       
        "meanings": {                            
            "ger": [                               
                "welch"                           
            ]                                      
        },                                       
        "ent_seq": "1920240"                     
    },
    {                           
        "pos": [                  
            "adj-i"                 
        ],                        
        "commonness": 1,        
        "misc": [],               
        "kanji": [                
            {                       
                "text": "柔らかい",       
                "commonness": 57      
            }                  
        ],                        
        "kana": [                 
            {                       
                "text": "やわらかい",      
                "romaji": "Yawarakai",
                "commonness": 30      
            }                       
        ],                        
        "meanings": {             
            "ger": [                
                "(1) weich",          
            ]                       
        },                        
        "ent_seq": "1605630"      
    }                                                                 
]


let searchDb = require('./searchDb')

let searchindex = require('./searchindex')

describe('Serverless DB', function() {
    let dbfolder = 'mochaTest'
    this.timeout(20000)
    before(function(done) {
        database.createDatabase(data, dbfolder, [
            { boost:'commonness' , options:{type:'int'}}, 
            { fulltext:'ent_seq' },
            // { fulltext:'kanji[].text' }, 
            // { fulltext:'kana[].romaji' }, 
            // { fulltext:'kana[].text' }, 
            { boost:'field1.rank' , options:{type:'int'}}, 
            { fulltext:'field1.text' }, 
            { fulltext:'kanji[].text' }, 
            { fulltext:'meanings.ger[]', options:{tokenize:true} },
            { fulltext:'meanings.eng[]', options:{tokenize:true} }, 
            { boost:'kanji[].commonness' , options:{type:'int'}}, 
            { boost:'kana[].commonness', options:{type:'int'} }
        ])
        .then(() => {
            if (!process.cwd().endsWith(dbfolder))
                process.chdir(process.cwd()+'/'+dbfolder)
            done()
        })
    })

    it('should search tokenized and levensthein', function() {
        return searchDb.searchDb('mochaTest', {search: {
            term:'majestätischer',
            path:'meanings.ger[]',
            levenshtein_distance:1,
            firstCharExactMatch:true
        }}).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(1)
    })

    it('should search without firstCharExactMatch', function() {
        return searchDb.searchDb('mochaTest', {search: {
            term:'najestätischer',
            path:'meanings.ger[]',
            levenshtein_distance:1
        }}).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(1)
    })

    it('should search word non tokenized', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchDb.searchDb('mochaTest', {search: {
            term:'偉容',
            path:'kanji[].text',
            levenshtein_distance:0,
            firstCharExactMatch:true
        }}).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(1)
    })

    it('should search on non subobject', function() {
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'1587690',
                path:'ent_seq',
                levenshtein_distance:0,
                firstCharExactMatch:true
            }
        }).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(1)
    })

    it('AND connect hits', function() {
        return searchDb.searchDb('mochaTest', {
            AND: [{search: { term:'Majestät', path:'meanings.ger[]'}},
                {search: { term:'majestic', path:'meanings.eng[]'}}]
        })
        .should.eventually.have.length(1)
    })

    it('AND missing hits', function() {
        return searchDb.searchDb('mochaTest', {
            AND: [{search: { term:'Majestät', path:'meanings.ger[]'}},
                {search: { term:'urge', path:'meanings.eng[]'}}]
        })
        .should.eventually.have.length(0)
    })

    it('OR Connect hits', function() {
        return searchDb.searchDb('mochaTest', {
            OR: [{search: { term:'Majestät', path:'meanings.ger[]'}},
                {search: { term:'urge', path:'meanings.eng[]'}}]
        })
        .should.eventually.have.length(2)
    })

    it('should extract corect texts', function() {
        let allValues = fs.readFileSync('./meanings.ger[]', 'utf-8').split('\n')
        expect(allValues).to.eql(['anblick','aussehen','begeisterung','der', 'der test','majestät','majestätischer','majestätischer anblick','majestätisches','majestätisches aussehen','test', 'weich', 'welch','wille','wollen'])
    })

    it('should detect the path to anchor', function() {
        expect(util.getStepsToAnchor('kanji[].text')).to.eql(['kanji[]', 'kanji[].text'])
        expect(util.getStepsToAnchor('meanings.ger[]')).to.eql(['meanings.ger[]', 'meanings.ger[]'])
    })

    it('should search and boost', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'意慾',
                path:'kanji[].text',
                levenshtein_distance:0,
                firstCharExactMatch:true
            },
            boost: {
                path:'kanji[].commonness',
                fun:Math.log,
                param: 1
            }
        }).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(2)
    })

    it.only('should search and double boost', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'awesome',
                path:'field1.text',
                levenshtein_distance:0,
                firstCharExactMatch:true
            },
            boost: [
                {
                    path:'commonness',
                    fun:Math.log,
                    param: 1
                },
                {
                    path:'field1.rank',
                    fun:rank => { if(!rank)return 0; return 10 / rank}
                }
            ]
        }).then(res => {
            console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(2)
    })


    it('should search and boost anchor', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'意慾',
                path:'kanji[].text',
                levenshtein_distance:0,
                firstCharExactMatch:true
            },
            boost: {
                path:'commonness',
                fun:Math.log,
                param: 1
            }
        }).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.deep.property('[0].commonness', 500)
        // .should.eventually.have.length(2)
    })


    it('should suggest', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchindex.suggest('meanings.ger[]', 'majes').then(res => {
            console.log(JSON.stringify(res, null, 2))
            return Object.keys(res)
        })
        .should.eventually.have.length(5)
    })

    it('should or connect the checks ', function() {
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'having a long',
                path:'meanings.eng[]',
                levenshtein_distance:1,
                firstCharExactMatch:true,
                startsWith:true,
                operator:'some'
            }
        }).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.length(1)
    })

    it('should rank exact matches pretty good', function() {
        console.log("test search123123123")
        console.log(process.cwd())
        return searchDb.searchDb('mochaTest', {
            search: {
                term:'weich',
                path:'meanings.ger[]',
                levenshtein_distance:1,
                firstCharExactMatch:true
            },
            boost: {
                path:'commonness',
                fun:Math.log,
                param: 1
            }
        }).then(res => {
            // console.log(JSON.stringify(res, null, 2))
            return res
        })
        .should.eventually.have.deep.property('[0].meanings.ger[0]', '(1) weich')
    })

    // it('should search', function() {
    //     let mainids = Array.from(require('./loadUint32')('./meanings.ger.mainids'))
    //     console.log(mainids)
    // })

    after(function() {
        // deleteFolderRecursive('mochaTest')
    })

})
