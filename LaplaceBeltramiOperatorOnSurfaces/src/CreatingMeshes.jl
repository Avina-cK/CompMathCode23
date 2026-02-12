
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

function create_mesh(n_samples::Int)
    
    nx=n_samples;
    ny=n_samples;
    nz=n_samples;
    samples = (nx, ny, nz)
    origin  = SVector(-1.01, -1.01, -1.01)
    widths  = SVector( 2.26,  2.251,  2.251)

    # Available algorithms: NaiveSurfaceNets, MarchingCubes, MarchingTetrahedron
    points, faces = isosurface(f,
        NaiveSurfaceNets(
            iso=0.0,insidepositive=true
            ),
        origin = origin,
        widths = widths,
        samples = samples
    ) 

    gb_points = Point.(points)

    gb_faces = QuadFace.(Tuple.(faces))

    #gb_mesh = GeometryBasics.Mesh(gb_points, gb_faces)

    @save "surface_$(nx)$(ny)$(nz).jld2" gb_points gb_faces
    #save("surface_$(nx)$(ny)$(nz).obj", gb_mesh)

end

#= Example use:
create_mesh(10)
=#
