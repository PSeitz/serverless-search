'use strict'

let service = {}

service.database = () => require("./database")

service.searchdb = () => require("./searchdb")

service.jsonfilter = () => require("./jsonfilter")

service.searchindex = () => require("./searchindex")

module.exports = service
