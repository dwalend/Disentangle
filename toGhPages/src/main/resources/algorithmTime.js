// Set the dimensions of the canvas / graph

function plotIt(filename)
{
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

    // Adds the svg canvas
    var svg = d3.select("body")
        .append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
        .append("g")
            .attr("transform",
                  "translate(" + margin.left + "," + margin.top + ")");

    // Get the data
    console.log(filename)

    d3.csv(filename, function(error, data) {

        console.log("data is "+data)

        // Scale the range of the data
        x.domain([0, d3.max(data, function(d) { return Number(d.nodes); })]);
    //    y.domain([0, d3.max(data, function(d) { return Number(d.measured); })]);
        y.domain([0, d3.max(data, function(d) { return Number(d.expected); })]);

        // Add the valueline path.
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
    });
}

//todo next call from scala.js and get it to a file. (Think it'll be a blank white box?)
function plotToPng() {
/*
  var canvas = d3.select('body').append('canvas').node();
  canvas.width = 100;
  canvas.height = 100;
  var ctx = canvas.getContext('2d');
  ctx.drawImage(img, 0, 0);
*/
  var canvas = d3.select('body').append('canvas').node();
  var canvasUrl = canvas.toDataURL("image/png");

//  console.log(canvasUrl)

  return canvasUrl

}

dataToPng = function(filename) {
    plotIt(filename)

    var pngPlot = plotToPng();
//    var fs = require('fs');
//    fs.write("dijkstra.png",plotToPng(),'w') ;

    return pngPlot;
}

hello = function() {
    console.log("hello from js")

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

		// save result in an html file, we could also keep it in memory, or export the interesting fragment into a database for later use
		var svgsrc = window.document.innerHTML
		fs.writeFile('index.html', svgsrc, function(err) {
			if(err) {
				console.log('error saving document', err)
			} else {
				console.log('The file was saved!')
			}
		})
	} // end jsDom done callback
})


}
