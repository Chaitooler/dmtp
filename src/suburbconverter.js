var fs = require('fs');
var parse = require('csv-parse');
var async = require('async');
var csv = require('csvtojson');
var https = require('https');
var jsonfile = require('jsonfile');

var inputFile='./data/sucursaleslatlong.json';
var outputFile='./data/barrios.json';

/*
console.log('converting');

var parser = parse({delimiter: ','}, function (err, data) {
  async.eachSeries(data, function (line, callback) {
    // do something with the line
        console.log('callback')
        console.log(line)
  })
});
fs.createReadStream(inputFile).pipe(parser);
*/

var appending = []

csv()
.fromFile(inputFile)
.then((jsonobjs)=> {
    console.log(jsonobjs)

    jsonobjs.forEach((jsonobj) => {
        let url = 
        'https://api.opencagedata.com/geocode/v1/json?key=6a672e7c63824ffea1bd8d2d3e2f5b6b&q='
        +jsonobj.lat  //'-34.55212'
        +'%2C+'
        +jsonobj.lng  //'-58.49841'
        +'&pretty=1&no_annotations=1';

        console.log(url);

        https.get(url, function(res) {
            var body = '';

            res.on('data', (chunk) => {
                body += chunk
            })

            res.on('end', () => {
                try {
                    var fbResponse = JSON.parse(body)
                    console.log(fbResponse)

                    jsonobj['barrio'] = fbResponse.results[0].components.suburb;
                    appending.push(jsonobj)

                    jsonfile.writeFileSync(outputFile, jsonobj, {flag: 'a'})
                } catch(error) {
                    console.log(error)
                }    
            })
        })

    })

})





