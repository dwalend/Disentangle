var useLogOrLinear = function(useLog) {
    if (useLog) return d3.scale.log().base(2)
    else return d3.scale.linear()
}

var createXScale = function(useLog,w,h,padding,latestData) {
    return useLogOrLinear(useLog)
            .domain([d3.min(latestData, function(d) { return Number(d.nodes); }),
                        d3.max(latestData, function(d) { return Number(d.nodes); })])
            .range([padding, w - padding * 2])
}

var createYScale = function(useLog,w,h,padding,latestData) {
    return useLogOrLinear(useLog)
             .domain([d3.min(latestData, function(d) { if(Number(d.measured) < Number(d.expected)) return Number(d.measured)
                                                        else return Number(d.expected)}),
                    d3.max(latestData, function(d) { if(Number(d.measured) > Number(d.expected)) return Number(d.measured)
                                                        else return Number(d.expected)
              })])
             .range([h - padding, padding])
}

var createXAxis = function(w,h,padding,xScale,svg) {
    var xAxis= d3.svg.axis()
              .scale(xScale)
              .orient("bottom")
              .tickFormat(function (d) {
                return xScale.tickFormat(6,d3.format(",d"))(d)
                })
    svg.append("g")
            .attr("class", "axis")
            .attr("transform", "translate(0," + (h - padding) + ")")
            .call(xAxis)

     svg.append("text")
        .attr("transform", "translate(" + (w / 2) + " ," + (h - 2*padding/3) + ")")
        .style("text-anchor", "middle")
        .text("Nodes")

    return xAxis
}

var createYAxis = function(w,h,padding,yScale,svg) {
    var yAxis = d3.svg.axis()
              .scale(yScale)
              .orient("left")

    svg.append("g")
                .attr("class", "axis")
                .attr("transform", "translate(" + padding + ",0)")
                .call(yAxis)

     svg.append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 0 + padding/2)
        .attr("x", 0 - (h / 2))
        .attr("dy", "1em")
        .style("text-anchor", "middle")
        .text("Seconds")

    return yAxis
}

var createDots = function(dotSetName,xName,yName,color,dataSet,xScale,yScale,svg) {
    svg.selectAll(dotSetName)
       .data(dataSet)
       .enter()
       .append("circle")
       .attr("cx", function(d) {
            return xScale(Number(d[xName]));
       })
       .attr("cy", function(d) {
            return yScale(Number(d[yName]));
       })
       .attr("r", 4)
       .attr("fill", color);
}

var createLine = function(lineName,xName,yName,color,dataSet,xScale,yScale,svg) {
            var valueline = d3.svg.line()
                .x(function(d) { return xScale(d[xName]) })
                .y(function(d) { return yScale(d[yName]) })

            svg.append("path")
                .attr("id", lineName)
                .attr("d", valueline(dataSet))
                .attr("stroke", color);
}

var plotResults = function(useLog,containerId,primaryFile,otherFile) {

    var w = 1600;
    var h = 900;
    var padding = 120;
    var latestData = [];
    var standardData = [];
    var nanoSecond = Math.pow(10,-9)

    queue()
        .defer(d3.csv, primaryFile, function(d) {
            latestData.push({"nodes":+d.nodes, "measured":+d.measured*nanoSecond, "expected":+d.expected*nanoSecond})
            })
        .defer(d3.csv, otherFile, function(d) {
            standardData.push({"nodes":+d.nodes, "measured":+d.measured*nanoSecond, "expected":+d.expected*nanoSecond})
            })
        .await(ready)

    function ready(error,other) {
         if(error) throw error

        //Create scale functions
        var xScale = createXScale(useLog,w,h,padding,latestData)

        var yScale = createYScale(useLog,w,h,padding,latestData)

        //Create SVG element
        var svg = d3.select("body").select(containerId)
                    .append("svg")
                    .attr("width", w)
                    .attr("height", h)

        //Define X axis
        var xAxis = createXAxis(w,h,padding,xScale,svg)
        //Define Y axis
        var yAxis = createYAxis(w,h,padding,yScale,svg)

        createLine("expectedLine","nodes","expected","red",latestData,xScale,yScale,svg)

        createDots("measured","nodes","measured","blue",latestData,xScale,yScale,svg)
        createDots("expected","nodes","expected","red",latestData,xScale,yScale,svg)
        createDots("jung","nodes","measured","green",standardData,xScale,yScale,svg)

    }
}