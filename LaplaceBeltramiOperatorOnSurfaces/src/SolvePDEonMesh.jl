
using Gridap
using Gridap.Geometry
using Gridap.FESpaces
using LinearAlgebra
using SparseArrays
using GridapGmsh

# set current directory
# cd(...)
include("include/basic_functions.jl")

#cd(".../isomeshes/")
mn = 10
mesh_filename="surface_$(mn)$(mn)$(mn)_tri";
# Create a DiscreteModel from the GMSH mesh file
mesh = GmshDiscreteModel("isomeshes/$(mesh_filename).msh")

# Define the true solution
u_exact(x) = x[1] * x[2]

# Define finite element space
order = 1
reffe = ReferenceFE(lagrangian, Float64, order)

# Create test space. constraint for closed surface: mean=0
Φ = TestFESpace(mesh, reffe, conformity=:H1,constraint=:zeromean)

# Create trial space
U = TrialFESpace(Φ)

# Define triangulation and measure
Ω = Triangulation(mesh)
degree = 2 * order
dΩ = Measure(Ω, degree)

# Create the exact solution 
# CellField: way of calculating functions on a mesh, from Gridap
u_ex = CellField(u_exact, Ω)    # true solution on each vertex
f_Ω = CellField(rhs_f, Ω)       # RHS, f, on each vertex
n_Ω = get_normal_vector(Ω)      

# Laplace-Beltrami operator
function  ∇ₛ(u)
    return ∇(u) - dot(∇(u), n_Ω)*n_Ω
end

# ---- Define the weak form ------------#
a(u, ϕ) = ∫(∇ₛ(u) ⊙ ∇ₛ(ϕ))*dΩ 
# RHS:
l(ϕ) = ∫( f_Ω * ϕ)*dΩ 

# --- Create the affine FE operator ---#
println("Assembling system...")
op = AffineFEOperator(a, l, U, Φ)

# ---- Solving system -----------------#
println("Solving system...")
uₕ = solve(op)

# ---- Calculating error --------------#
e = u_exact - uₕ
l2_error = sqrt(sum(∫(e * e)*dΩ))/sqrt(sum(∫(u_ex * u_ex)*dΩ))
h1_error = sqrt(sum( ∫( e*e + ∇ₛ(e)⋅∇ₛ(e) )*dΩ ))

println("\nResults:")
println("L2 error: ", l2_error)
println("H1 error: ", h1_error)

# Calculating the residual
residual = a(uₕ, uₕ) - l(uₕ)
println("Residual: ", residual)

#cd(".../results/")
# --- Write solution to file -------------------------------------------------------------- #
writevtk(Ω, "soln_$(mesh_filename)", cellfields=["uₕ" => uₕ, "u_exact" => u_ex, "error" => e])
println("\nSolution written to soln_$(mesh_filename).vtu")
