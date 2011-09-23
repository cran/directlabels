import rsvg
import cairo
def convert(svg_file):
    svg=rsvg.Handle(svg_file)
    ps_file=svg_file.replace(".svg",".ps")
    d=svg.get_dimension_data()
    ps=cairo.PSSurface(ps_file,d[0],d[1])
    context=cairo.Context(ps)
    svg.render_cairo(context)
    ps.finish()
if __name__=="__main__":
    import sys
    for f in sys.argv[1:]:
        print f
        convert(f)
