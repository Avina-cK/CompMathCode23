
#cd("Path/To/src")

#include("QColourGradients.jl");


using SciMLBase
using LinearAlgebra                 # makes using matrices easier
using DifferentialEquations         # contains ODE solvers
using SparseArrays                  # for possible sparse Jacobian construction 
using Plots, LaTeXStrings, Printf   # required to plot results and print phrases neatly
using JLD2                          # to save U(final), V(final)
using Logging: global_logger            # to show estimated time of completion
using TerminalLoggers: TerminalLogger   # show ETA in terminal 
using SparseConnectivityTracer, ADTypes # To find the sparse jacobian matrix
using Random                        # generating initial conditions noise

global_logger(TerminalLogger())

default(fontfamily="Computer Modern", size = (1000, 800)
                , tickfontsize=12
                , guidefontsize=14
                , titlefontsize=16
                , colorbar_tickfontsize=11)

# ----------------Function to simulate ODE for given parameters ------------------------------------------------------------
function Sim_SparseODE_SchnakenbergSys_64(Du::Float64, Dv::Float64, a::Float64, b::Float64, U0::Matrix{Float64}, V0::Matrix{Float64}, nsteps::Int64=Int64(10000);
                                    solvername = TRBDF2(), deltat::Float64= 0.001, saveat_t::Float64 = 0.1, togif::Bool=true, SaveUVf::Bool=false, colourgrad = les7, plotwidth::Int=1000, extra_iter::Int=1)
    # Dimensions of system
    nx = size(U0, 1);
    ny = size(V0, 1); 
    
    # ------------- defining ODE --------------------------
    function Schnakenberg_2d_ODE(du, u, p, t)
        Dᵤ, Dᵥ, a, b, dx = p
        Dᵤ = Dᵤ/(dx^2)
        Dᵥ = Dᵥ/(dx^2)
        ∇²u = laplacian_neumann_5pt_2(u[:,:,1])
        ∇²v = laplacian_neumann_5pt_2(u[:,:,2])
        for I in CartesianIndices((nx, ny))
            i,j = Tuple(I)
            du[i,j,1] = (Dᵤ *∇²u[i,j]) + a - u[i,j,1] + ((u[i,j,1]^2) * u[i,j,2])
            du[i,j,2] = (Dᵥ *∇²v[i,j]) + b - ((u[i,j,1]^2) * u[i,j,2])
        end
    end

    dx::Float64= 1.0;
    
    # Initial conditions as (U0, V0) as a 3-d array.
    u0 = Array{eltype(U0)}(undef, size(U0, 1), size(U0, 2), 2)
    u0[:, :, 1] .= U0
    u0[:, :, 2] .= V0

    # parameters as a vector
    p = (Du, Dv, a, b, dx)
    params = @sprintf("%.3f, %.3f, %.3f, %.3f",
                    Du, Dv, a, b)
    
    final_T = Float64(nsteps*deltat)

    # Detecting sparsity of Jacobian
    detector = TracerSparsityDetector()
    du0 = copy(u0)
    jac_sparsity = ADTypes.jacobian_sparsity(
    (du, u) -> Schnakenberg_2d_ODE(du, u, p, 0.0), du0, u0, detector)
    @info "Found spare jacobian!"

    f = SciMLBase.ODEFunction(Schnakenberg_2d_ODE; jac_prototype = float.(jac_sparsity))
    prob_ode_schnakenberg_2d_wSparse = ODEProblem(f, u0, (0.0, final_T), p)

    @info "Defined ODE for [Du, Dv, a, b, T_final] = [$(params), $(final_T)]"    
    @info "Solving system with $(solvername)"

    if togif
        save_every=true
    else  save_every=false
    end

    # solving the defined system
    trajectory = solve(prob_ode_schnakenberg_2d_wSparse, solvername, saveat = saveat_t, save_on = save_every, dt=deltat, progress=true);

    # Final state of system
    Uf = trajectory[:,:,1,end]
    Vf = trajectory[:,:,2,end] 
    @info "Completed Simulation"
    
    # Extract solver name for output files name
    solvernamestring_f =  string(solvername);
    idx = findfirst(c -> c == '(' || c == '{', solvernamestring_f)
    solvernamestring = idx === nothing ? solvernamestring_f : solvernamestring_f[1:idx-1];
    # Create parameter string for output files name
    params_fn = @sprintf("%05.2f_%.2f_%.2f_%.2f_%d", Du, Dv, a, b, (nsteps*extra_iter))
    
    # function to create frame with appropriate titles for u, v
    function frameV(V, step, whichvar; subsize = (plotwidth, 800))
            if whichvar==1
                plottitle = L"\textit{u(T)}, " *"       "* "time ="*" $(round(step,digits=2)) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]"
            else 
                plottitle = L"\textit{v(T)}, " *"       "* "time ="*" $(round(step,digits=2)) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]"
            end
        heatmap(V; c = colourgrad, title = plottitle, titlelocation=:left,
                aspect_ratio = 1, colorbar = true, axis = nothing,
                xlims = (1, nx), ylims = (1, ny),
                size = subsize, xticks = false, yticks = false
                )
    end

    @info "Generating results"
    
    # Saving final states
    pVf = frameV(Vf, final_T, 2)
    savefig(pVf, "schnakenberg64_V_$(params_fn)_$(solvernamestring).png")
    pUf =  frameV(Uf, final_T, 1)
    savefig(pUf, "schnakenberg64_U_$(params_fn)_$(solvernamestring).png")

    # Generating GIFs
    if togif
        savedsteps = size(trajectory,4)
        anim1 = Animation()
        anim2 = Animation()
        for step in 1:savedsteps
            tstep = step * saveat_t
            U = trajectory[:,:,1,step]
            V = trajectory[:,:,2,step]
            frame = frameV(V, tstep,2)
            frame |> frame -> frame |> frame -> frame 
            frame |> frame -> frame 
            frame |> frame -> frame 
            frameV(V, tstep, 2) |> frame -> Plots.frame(anim1, frame)
            frame = frameV(U, tstep,1)
            frame |> frame -> frame |> frame -> frame 
            frame |> frame -> frame 
            frame |> frame -> frame 
            frameV(U, tstep, 1) |> frame -> Plots.frame(anim2, frame)  
        end
        @info "Generated GIFs"
        gif(anim1, "schnakenberg64_V_$(params_fn)_$(solvernamestring).gif"; fps = 10)
        gif(anim2, "schnakenberg64_U_$(params_fn)_$(solvernamestring).gif"; fps = 10)
    end
    
    # Save final state as a matrix that can easily be reloaded
    if SaveUVf
        @save "Uf_$(params_fn).jld2" Uf;
        @save "Vf_$(params_fn).jld2" Vf;
    end
    @info "Simulation complete for [$(Du), $(Dv), $(a), $(b), $(nsteps)] using $(solvername)"
end;

# ------------------------ Example Use ---------------------------------------
#=
eq_gridsize::Int64 = 100;

Dᵤ::Float64 = 0.20;
Dᵥ::Float64 = 20.0;
a::Float64 = 0.025;
b::Float64 = 1.5;

eqpt_u::Float64 = Float64(a+b);             
eqpt_v::Float64 = Float64(b/((a+b)^2));

using Random
Random.seed!(314)
purturbU0 =  Float64(0.1f0) .* randn(Float64, eq_gridsize, eq_gridsize);
U0 = fill(eqpt_u, eq_gridsize, eq_gridsize) + purturbU0;

Random.seed!(3142)
purturbV0 = Float64(0.1f0) .* randn(Float64, eq_gridsize, eq_gridsize);
V0_base = fill(eqpt_v,   eq_gridsize, eq_gridsize);
V0 = V0_base + purturbV0;

# -- Output folder ---------------------------------------------
outputparams = @sprintf("%.3f_%.3f_%.3f_%.3f", Dᵤ, Dᵥ, a, b)
output_dir = "outputs/schnakenberg64_$(outputparams)_$(eq_gridsize)"
mkpath(output_dir)
cd(output_dir)

nsteps::Int64 = 5000
solvername_val = TRBDF2()
extra_iter_val = 1;
# -- Simulation ----------------------------------------------
Sim_SparseODE_SchnakenbergSys_64(Dᵤ, Dᵥ, a, b, U0, V0, nsteps
                ; colourgrad=gm7, solvername=solvername_val, togif=false, SaveUVf=true)
=#