
# Julia 1.10.10 (Julia LTS)
# Meshingv0.5.6 conflicts with current versions of Meshes

# cd(".../isomeshes")

using Meshing
using StaticArrays
using LinearAlgebra
using MeshIO
using GeometryBasics
using JLD2

f(v) = (v[1] - v[3]^2)^2 + v[2]^2 + v[3]^2 - 1

nx = 35
ny = 35
nz = 35
origin  = SVector(-2.5, -1.25, -1.25)
widths  = SVector( 5.0,  2.5,  2.5)

for i in 10:5:50
    nx=i;
    ny=i;
    nz=i;
    samples = (nx, ny, nz)

    # Available algorithms: NaiveSurfaceNets, MarchingCubes, MarchingTetrahedron
    points, faces = isosurface(f,
        MarchingCubes(
            iso=0.0,insidepositive=true
            ),
        origin = origin,
        widths = widths,
        samples = samples
    ) 

    gb_points = Point.(points)

    gb_faces = QuadFace.(Tuple.(faces))

    gb_mesh = GeometryBasics.Mesh(gb_points, gb_faces)

    @save "surface_$(nx)$(ny)$(nz).jld2" gb_points gb_faces
    #save("surface_$(nx)$(ny)$(nz).obj", gb_mesh)

end
