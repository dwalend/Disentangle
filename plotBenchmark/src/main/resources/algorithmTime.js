

function plotToPng() {
  var canvas = d3.select('body').append('canvas').node();
  var canvasUrl = canvas.toDataURL("image/png");

  return canvasUrl
}

dataToPng = function(filename) {

    process.argv.forEach(function(val, index, array) {
      console.log(index + ': ' + val);
    });

    plotIt(filename)

//    var pngPlot = plotToPng();
//    var fs = require('fs');
//    fs.write("dijkstra.png",plotToPng(),'w') ;

//    return pngPlot;
}




plotIt = function(filename) {
//see https://gist.github.com/mef/7044786
//and http://mango-is.com/blog/engineering/pre-render-d3-js-charts-at-server-side.html
//and http://d3export.housegordon.org
//and http://bl.ocks.org/vicapow/758fce6aa4c5195d24be
//and http://robballou.com/2013/creating-an-svg-file-with-d3-and-node-js/

    var d3 = require('d3')
        , jsdom = require('jsdom')
        , fs = require('fs')
	    , htmlStub = '<html><head></head><body><div id="div1"></div><div id="div2"></div><script src="js/d3.v3.min.js"></script></body></html>'

    jsdom.env({
        features : { QuerySelector : true }
        , html : htmlStub
        , done : function(errors, window) {

            var margin = {top: 30, right: 20, bottom: 30, left: 100},
                width = 600 - margin.left - margin.right,
                height = 270 - margin.top - margin.bottom;

            // Set the ranges
            var x = d3.scale.linear().range([0, width]);
            var y = d3.scale.linear().range([height, 0]);

            // Define the axes
            var xAxis = d3.svg.axis().scale(x)
                .orient("bottom").ticks(5);

            var yAxis = d3.svg.axis().scale(y)
                .orient("left").ticks(5);

            // Define the line
            var valueline = d3.svg.line()
                .x(function(d) { return x(d.nodes); })
                .y(function(d) { return y(d.measured); });

            var div1 = window.document.querySelector('#div1')
                , div2 = window.document.querySelector('#div2')
                , body = window.document.querySelector('body')
                , circleId = 'a2324'  // say, this value was dynamically retrieved from some database

            net.walend.graph.results.PlotTime().plotD3(div2)

            // Adds the svg canvas
            var svg = d3.select(div1)
                .append("svg")
                    .attr("width", width + margin.left + margin.right)
                    .attr("height", height + margin.top + margin.bottom)
                .append("g")
                    .attr("transform",
                          "translate(" + margin.left + "," + margin.top + ")");


            fs.readFile(filename, 'utf8', function (err, filling) {

                var data = d3.csv.parse(filling);

                // Scale the range of the data
                // todo use the max of all numbers that may be plotted
                x.domain([0, d3.max(data, function(d) { return Number(d.nodes); })]);
            //    y.domain([0, d3.max(data, function(d) { return Number(d.measured); })]);
                y.domain([0, d3.max(data, function(d) { return Number(d.expected); })]);

                // todo Add the valueline path.
            //    svg.append("path")
            //        .attr("class", "line")
            //        .attr("d", valueline(data));

                // Add the scatterplot of the measurements
                svg.selectAll("dot")
                    .data(data)
                  .enter().append("circle")
                    .attr("r", 3.5)
                    .attr("fill","blue")
                    .attr("cx", function(d) { return x(d.nodes); })
                    .attr("cy", function(d) { return y(d.measured); });

                // Add the scatterplot of expected values
                svg.selectAll("dot")
                    .data(data)
                  .enter().append("circle")
                    .attr("r", 3.5)
                    .attr("fill","red")
                    .attr("cx", function(d) { return x(d.nodes); })
                    .attr("cy", function(d) { return y(d.expected); });

                // Add the X Axis
                svg.append("g")
                    .attr("class", "x axis")
                    .attr("transform", "translate(0," + height + ")")
                    .call(xAxis);

                // Add the Y Axis
                svg.append("g")
                    .attr("class", "y axis")
                    .call(yAxis);

                var svgsrc = window.document.documentElement.innerHTML
                fs.writeFile('chart.html', svgsrc, function(err) {
                    if(err) {
                        console.log('error saving document', err)
                    } else {
                        console.log('The file was saved!')
                    }
                })

            });

    }})

    var result = net.walend.graph.results.PlotTime().callBack("Hello from javascript!")

    console.log(result)
}

greenCircle = function() {

//see https://gist.github.com/mef/7044786 and http://mango-is.com/blog/engineering/pre-render-d3-js-charts-at-server-side.html

// pre-render d3 charts at server side
var d3 = require('d3')
	, jsdom = require('jsdom')
	, fs = require('fs')
	, htmlStub = '<html><head></head><body><div id="dataviz-container"></div><script src="js/d3.v3.min.js"></script></body></html>'

jsdom.env({
	features : { QuerySelector : true }
	, html : htmlStub
	, done : function(errors, window) {
	// this callback function pre-renders the dataviz inside the html document, then export result into a static file

		var el = window.document.querySelector('#dataviz-container')
			, body = window.document.querySelector('body')
			, circleId = 'a2324'  // say, this value was dynamically retrieved from some database

		// generate the dataviz
		d3.select(el)
			.append('svg:svg')
				.attr('width', 600)
				.attr('height', 300)
				.append('circle')
					.attr('cx', 300)
					.attr('cy', 150)
					.attr('r', 30)
					.attr('fill', '#26963c')
					.attr('id', circleId) // say, this value was dynamically retrieved from some database

		// make the client-side script manipulate the circle at client side)
		var clientScript = "d3.select('#" + circleId + "').transition().delay(1000).attr('fill', '#f9af26')"

		d3.select(body)
			.append('script')
				.html(clientScript)

	// Get the d3js SVG element
//	var tmp = document.getElementById("ex1");
	var svg = body.getElementsByTagName("svg")[0];
	// Extract the data as SVG text string - see http://robballou.com/2013/creating-an-svg-file-with-d3-and-node-js/ and http://bl.ocks.org/vicapow/758fce6aa4c5195d24be and http://d3export.housegordon.org
	var xmldom = require('xmldom');
	var svg_xml = (new xmldom.XMLSerializer).serializeToString(svg);

		// save result in an html file, we could also keep it in memory, or export the interesting fragment into a database for later use

		//this grabs the html for the page!
 //       var svgsrc = window.document.documentElement.innerHTML
		fs.writeFile('greenCircle.svg', svg_xml, function(err) {
			if(err) {
				console.log('error saving document', err)
			} else {
				console.log('The file was saved!')
			}
		})
	} // end jsDom done callback
})


}

hello = function() {
    console.log("hello from js")
}
