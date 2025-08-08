#cd("Path/To/src")
include("QColourGradients.jl");
include("Func_SolveSparseODE_SchnakenbergSys64.jl")
include("Laplacian.jl");

# change the following to outputfolder
main_dir = "Path/To/Output"

using Random
# ----- Parameters ------------------------------------------------------

world_size::Int64 = 200;      # world size
# dx = 1/world_size;

Dᵤ::Float64 = 1.0;         # Diffusion coefficient for U -> Du_given/(dx^2)
Dᵥ::Float64 = 8106.8;         # Diffusion coefficient for V -> Dv_given/(dx^2)
a::Float64 = 2.0;         # Parameter a
b::Float64 = 0.01;           # Parameter b

nsteps::Int64 = 200000;         # No. of steps to solve for 
solvername_val = TRBDF2();        # solver to use

Random.seed!(314)
purturbU0 =  Float64(0.01f0) .* randn(Float64, world_size, world_size);
    
Random.seed!(3142)
purturbV0 = Float64(0.01f0) .* randn(Float64, world_size, world_size);

extra_iter_val = 1;
 
# ------ Stable state, from linear analysis --------------------------------------------
eqpt_u::Float64 = Float64(a+b);             
eqpt_v::Float64 = Float64(b/((a+b)^2));

# ------ Creating initial conditions: (U₀,V₀) = (u*,v*) + noise ------------------------
U0 = fill(eqpt_u, world_size, world_size) + purturbU0;
V0 = fill(eqpt_v,   world_size, world_size) + purturbV0;

#------- Creating and setting output folder: Dᵤ_Dᵥ_a_b --------------------------------
outputparams = @sprintf("%05.2f_%.2f_%.3f_%.2f", Dᵤ, Dᵥ, a, b)
output_dir = main_dir*"/SolveSparseODE_output/schnakenberg64_$(outputparams)_$(nsteps)_$(world_size)"
mkpath(output_dir)
cd(output_dir)

# -- Simulation ----------------------------------------------
Sim_SparseODE_SchnakenbergSys_64(Dᵤ, Dᵥ, a, b, U0, V0, nsteps
        ; colourgrad=gm7, solvername=solvername_val
        , togif=false
        #, saveat_t=1.0
        , SaveUVf=false
        , extra_iter=extra_iter_val)
