
#cd("Path/To/src")

#include("src/Laplacian.jl");
#include("src/QColourGradients.jl");

using Plots, LaTeXStrings, Printf # required
using ProgressMeter # to show progress bar
using JLD2          # to save U(nsteps), V(nsteps)

function Func_SchnakenbergSys_32(Du::Float32, Dv::Float32, a::Float32, b::Float32, U0::Matrix{Float32}, V0::Matrix{Float32}
                                , nsteps::Int32=Int32(10000), eq_gridsize::Int32=Int32(200)
                                ; togif::Bool=true, SaveUVf::Bool=false, showprogress::Bool=false, colourgrad = les7, plotwidth::Int=1000)
    nx = eq_gridsize;
    ny = eq_gridsize;  
    dx::Float32= 1.0f0;      
    dt::Float32= 0.001f0;    
    U = U0 
    V = V0

    default(fontfamily="Computer Modern", size = (900, 800)
                , tickfontsize=12
                , guidefontsize=14
                , titlefontsize=16
                , colorbar_tickfontsize=11)
    params = @sprintf("%.3f, %.3f, %.3f, %.3f",
                    Du, Dv, a, b)
    function frameV(V, step; subsize = (900, 800))
        plottitle = L"\textit{v(T)}, " *"       "* "time step ="*" $(step) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]"
        heatmap(V; c = colourgrad, title = plottitle, titlelocation=:left,
                aspect_ratio = 1, colorbar = true, axis = nothing,
                xlims = (1, nx), ylims = (1, ny),
                size = subsize, xticks = false, yticks = false
                )
    end
    function frameU(V, step; subsize = (900, 800))
        plottitle = L"\textit{u(T)}, " *"       "* "time step ="*" $(step) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]"
        heatmap(U; c = colourgrad, title = plottitle, titlelocation=:left,
                aspect_ratio = 1, colorbar = true, axis = nothing,
                xlims = (1, nx), ylims = (1, ny),
                size = subsize, xticks = false, yticks = false
                )
    end
    if togif==true
        save_every = 200
        anim1 = Animation()
        anim2 = Animation()
    end
    if showprogress
        progressbar = Progress(nsteps; desc = "Running Schnakenberg (64) simulation", barlen = 40)
    end
    for step in 1:nsteps
        𝓛u = laplacian_neumann_5pt(U, dx)   
        𝓛v = laplacian_neumann_5pt(V, dx)

        f = a .- U .+ ((U.^2) .* V)
        g = b .- ((U.^2) .* V)

        U .+= dt .* (Du .* 𝓛u .+ f)
        V .+= dt .* (Dv .* 𝓛v .+ g)
        
        if togif==true
            if step % save_every == 0 || step == 1 || step == nsteps
                frame = frameV(V, step)
                frame |> frame -> frame |> frame -> frame 
                frame |> frame -> frame 
                frame |> frame -> frame 
                frameV(V, step) |> frame -> Plots.frame(anim1, frame)
                frame = frameU(U, step)
                frame |> frame -> frame |> frame -> frame 
                frame |> frame -> frame 
                frame |> frame -> frame 
                frameU(U, step) |> frame -> Plots.frame(anim2, frame)
            end
        end
        if showprogress
            next!(progressbar)
        end

    end
    @info "Generating results"
    params_fn = @sprintf("%05.2f_%.2f_%.2f_%.2f_%d", Du, Dv, a, b, nsteps)
    if togif==true
        gif(anim1, "schnakenberg64_V_$(params_fn).gif"; fps = 10)
        gif(anim2, "schnakenberg64_U_$(params_fn).gif"; fps = 10)
    end
    plottitleV = L"\textit{v(T)}, " *"       "* "time step ="*" $(nsteps) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]" * " , world size = $(eq_gridsize)"
    pVf = heatmap(V; c =colourgrad,  
                title = plottitleV,
                aspect_ratio = 1,
                colorbar = true,
                axis = nothing,
                xlims = (1, nx), ylims = (1, ny),
                size = (plotwidth, 800),
                xticks = false, yticks = false
                , titlelocation=:left
                )
    savefig(pVf, "schnakenberg64_V_$(params_fn).png")
    plottitleU = L"\textit{u(T)}, " *"       "* "time step ="*" $(nsteps) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$(params)]" * " , world size = $(eq_gridsize)"
    pUf = heatmap(U; c =colourgrad,  
                title = plottitleU,
                aspect_ratio = 1,
                colorbar = true,
                axis = nothing,
                xlims = (1, nx), ylims = (1, ny),
                size = (plotwidth, 800),
                xticks = false, yticks = false
                , titlelocation=:left
                )
    savefig(pUf, "schnakenberg64_U_$(params_fn).png")
    if SaveUVf==true
        @save "Uf_$(params_fn).jld2" U;
        @save "Vf_$(params_fn).jld2" V;
    end
    @info "Simulation complete for [$(Du), $(Dv), $(a), $(b), $(nsteps)]"
end;

#= Example use 

eq_gridsize::Int32 = 200;

Dᵤ::Float32 = 0.0;
Dᵥ::Float32 = 0.0;
a::Float32 = 0.2
b::Float32 = 1.5

eqpt_u::Float32 = Float32(a+b);             
eqpt_v::Float32 = Float32(b/((a+b)^2));
    
Random.seed!(314)
purturbU0 =  Float32(0.1f0) .* randn(Float32, eq_gridsize, eq_gridsize);
U0 = fill(eqpt_u, eq_gridsize, eq_gridsize) + purturbU0;

Random.seed!(3142)
purturbV0 = Float32(0.1f0) .* randn(Float32, eq_gridsize, eq_gridsize);
V0_base = fill(eqpt_v,   eq_gridsize, eq_gridsize);
V0 = V0_base + purturbV0;

nsteps::Int32 = 15000
Func_SchnakenbergSys_32(Dᵤ, Dᵥ, a, b, U0, V0, nsteps , eq_gridsize; togif=true)

=#