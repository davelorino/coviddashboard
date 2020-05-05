function(el) {
  var tooltip5 = Plotly.d3.select('#' + el.id + ' .svg-container')
    .append("div")
    .attr("class", "my-custom-tooltip-click");

  el.on('plotly_hover', function(d) {
    var pt = d.points[0];
    // Choose a location (on the data scale) to place the image
    // Here I'm picking the top-left corner of the graph
    var x = pt.xaxis.range[0];
    var y = pt.yaxis.range[1];
    // Transform the data scale to the pixel scale
    var xPixel = pt.xaxis.l2p(x) + pt.xaxis._offset;
    var yPixel = pt.yaxis.l2p(y) + pt.yaxis._offset;
    // Insert the base64 encoded image
    var img3 = "<img src='" +  pt.customdata[1] + "' width=800>";
    tooltip5.html(img3)
      .style("position", "absolute")
      .style("bottom", xPixel + "px")
      .style("bottom", yPixel + "px");
    // Fade in the image
    tooltip5.transition()
      .duration(0)
      .style("opacity", 1);
  });

  el.on('plotly_unhover', function(d) {
    
    tooltip5.transition().duration(300).style("opacity", 0);
    img3 = "<img src='~/NetBaseApi/coviddashboard/block.jpeg' width=10>";
    tooltip5.html(img3)
      .style("position", "absolute")
      .style("top-left", xPixel + "px")
      .style("top-left", yPixel + "px");
    // Fade in the image
    tooltip5.transition()
      .duration(300)
      .style("opacity", 1);
      tooltip5.transition()
      .duration(500)
      .style("opacity", 0);
  });
}

