#include("C:/Users/avina/Documents/2023_MSc_FAU/604_SeminarCourse/Code/include/converting_mesh_formats.jl")
cd("C:/Users/avina/Documents/2023_MSc_FAU/604_SeminarCourse/Code/isomeshes_NaiveSurfaceNet/")
using JLD2, GeometryBasics
using Meshes
using Gmsh.gmsh

function convert_initmesh(i)
    filename="surface_$(i)$(i)$(i)";
    @load "$(filename).jld2" gb_points gb_faces
    mesh = GeometryBasics.Mesh(gb_points, gb_faces)
    points = coordinates(mesh)
    tri_faces = TriangleFace{Int}[]
    for f in GeometryBasics.faces(mesh)
        a, b, c, d = f
        push!(tri_faces, TriangleFace(a, b, c))
        push!(tri_faces, TriangleFace(a, c, d))
    end
    tri_mesh = GeometryBasics.Mesh(points, tri_faces)

    gmsh.initialize()
    gmsh.model.add("tri_mesh")

    pts   = coordinates(tri_mesh)
    faces = GeometryBasics.faces(tri_mesh)

    # Initial surface mesh
    surf_tag = 1
    gmsh.model.addDiscreteEntity(2, surf_tag)

    # Vertices
    pts = coordinates(tri_mesh)

    node_tags = collect(1:length(pts))

    node_coords = Float64[]
    for p in pts
        append!(node_coords, (p[1], p[2], p[3]))
    end
    gmsh.model.mesh.addNodes(
        2, surf_tag,
        node_tags,
        node_coords
    )

    # Triangles
    elem_type = 2 

    elem_tags = UInt64.(1:length(faces))

    elem_conn = UInt64[]
    for f in faces
        append!(elem_conn, (f[1], f[2], f[3]))
    end
    gmsh.model.mesh.addElements(
        2, surf_tag,
        [elem_type],
        [elem_tags],
        [elem_conn]
    )

    # Save mesh
    gmsh.write("$(filename)_tri.msh")
    gmsh.finalize()
end
    
for i in 61:10:101
    convert_initmesh(i)
end
