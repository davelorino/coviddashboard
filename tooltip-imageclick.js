function(el) {
  var tooltip1 = Plotly.d3.select('#' + el.id + ' .svg-container')
    .append("div")
    .attr("class", "my-custom-tooltip");
    
    var img1 = "<img src='block.jpeg' width=10>";

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
    var img1 = "<img src='" +  pt.customdata[0] + "' width=350>";

    tooltip1.html(img1)
      .style("position", "absolute")
      .style("bottom", xPixel + "px")
      .style("right", yPixel + "px");
    // Fade in the image
    tooltip1.transition()
      .duration(0)
      .style("opacity", 1);
      
   
  });

  el.on('plotly_unhover', function(d) {
    // Fade out the image
    tooltip1.transition()
      .duration(300)
      .style("opacity", 0);

    tooltip1.html(img1)
      .style("position", "absolute")
      .style("bottom-right", xPixel + "px")
      .style("bottom-right", yPixel + "px");
    // Fade in the image
    tooltip1.transition()
      .duration(300)
      .style("opacity", 1);
    tooltip1.transition()
      .duration(500)
      .style("opacity", 0);
      
  });
}

