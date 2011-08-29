### Label the closest point on the convex hull of the data.
chull.grid <- list("outside.chull","empty.grid")

### Label the closest point on the alpha hull of the data.
ahull.grid <- list("outside.ahull","empty.grid")

### Search the plot region for a label position near the center of
### each point cloud.
smart.grid <- list("big.boxes","empty.grid")

### Draw a regression line through the data, then draw perpendicular
### lines to the extreme points of each point cloud.
perpendicular.grid <- list("perpendicular.lines","big.boxes","empty.grid")

### Label each point cloud near the extremities of the plot region.
extreme.grid <- list("extreme.points","big.boxes","empty.grid")

