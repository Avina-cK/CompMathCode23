
cd("Path/To/Parent/Folder/With/Other/Scripts")

using Plots, LaTeXStrings, ProgressMeter, Random

include("Laplacian.jl");
include("QColourGradients.jl");

# ─────────────── Model parameters ─────────────────────────────────
eq_gridsize= 200;  # grid size  
nx = eq_gridsize;  # grid size, x axis
ny = eq_gridsize;  # grid size, y axis
dx= 1.0f0;       # grid spacing (arbitrary units)

Du= 0.2f0;
Dv= 20.0f0;      # diffusion rates
a = 0.15f0;      # parameter a(>0)∈ℝ
b = 0.52f0;      # parameter b(>0)∈ℝ

dt = 0.001f0;    # stable step, safely below 0.00625
nsteps = 1000000;   # run longer to reach the pattern

# ────────────────────── Initial conditions ───────────────────────
# equilibrium point from linear analysis: (eqpt_u, eqpt_v)
eqpt_u = Float32(a+b);             
eqpt_v = Float32(b/((a+b)^2));

Random.seed!(314)
# Initial U conditions: 1.0 + random noise
#U = ones(Float32, nx, ny) .+ 0.01f0 .* randn(Float32, nx, ny)
U = fill(eqpt_u,   nx, ny) .+ 0.1f0 .* randn(Float32, nx, ny)

Random.seed!(31415)
# Initial V conditions: equilibrium point (v) + random noise
V = fill(eqpt_v,   nx, ny) .+ 0.1f0 .* randn(Float32, nx, ny)

# ────────── Plotting defaults ────────────────────────────────────
default(fontfamily="Computer Modern", size = (800, 800)
            , tickfontsize=12
            , guidefontsize=14
            , titlefontsize=16
            , colorbar_tickfontsize=11)
# function to plot frames of final .gif
function frameV(V, step; subsize = (800, 800))
    plottitle = L"\textit{v(T)}, " *"       "* "time step ="*" $(step) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$Du, $Dv, $a, $b]"
    heatmap(V; c = nb4_bpyw, title = plottitle, titlelocation=:left,
            aspect_ratio = 1, colorbar = true, axis = nothing,
            xlims = (1, nx), ylims = (1, ny),
            size = subsize, xticks = false, yticks = false
            #, clims=(0.2, 1.0)
            )
end

# ── simulation with frame capture and progress bar ────────────────
togif = true
if togif==true
    save_every = 200
    anim = Animation()
end
progressbar = Progress(nsteps;
                desc = "Running Schnakenberg simulation",
                barlen = 40) 
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
            frame |> frame -> frame |> frame -> frame # force evaluation to avoid lazy update
            frame |> frame -> frame # fallback (Redundant but sometimes helps render properly)
            frame |> frame -> frame # Plots.jl can drop some frames, this nudges it
            frameV(V, step) |> frame -> Plots.frame(anim, frame)
        end
    end
    next!(progressbar)
end

@info "Simulation complete"
if togif==true
    gif(anim, "schnakenberg_V_$(nsteps)_$(Du)_$(Dv)_$(a)_$(b).gif"; fps = 10)
end
plottitleV = L"\textit{v(T)}, " *"       "* "time step ="*" $(nsteps) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$Du, $Dv, $a, $b]"
pVf = heatmap(V; c =nb4_bpyw,  
             title = plottitleV,
             aspect_ratio = 1,
             colorbar = true,
             axis = nothing,
             xlims = (1, nx), ylims = (1, ny),
             size = (800, 800),
             xticks = false, yticks = false
             , titlelocation=:left
             )

savefig(pVf, "schnakenberg_V_$(nsteps)_$(Du)_$(Dv)_$(a)_$(b).png")
