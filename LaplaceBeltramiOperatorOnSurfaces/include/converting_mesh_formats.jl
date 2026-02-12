using GeoIO
function converting_mesh(mesh_name, inputformat, outputformat="msh")
        
    geotable = GeoIO.load("$(mesh_name).$(inputformat)")
    return GeoIO.save("$(mesh_name).$(outputformat)", geotable)

end

convert_to_msh(mesh_name, inputformat) = converting_mesh(mesh_name, inputformat, "msh")
vtp_to_msh(mesh_name) = converting_mesh(mesh_name, "vtp","msh");
ply_to_msh(mesh_name) = converting_mesh(mesh_name, "ply","msh");

println("imported functions:")
println("converting_mesh(mesh_name, inputformat, outputformat),")
println("vtp_to_msh(mesh_name),")
println("ply_to_msh(mesh_name)")
