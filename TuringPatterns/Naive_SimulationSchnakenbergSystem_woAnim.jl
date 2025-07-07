
#cd("Path/To/src")

using Plots, Random
using Printf, LaTeXStrings  # for automatating and displaying plot title/ filename

include("src/Laplacian.jl");        #required
include("src/QColourGradients.jl"); #optional, but change colourgrad if not used

# ---------------- Model parameters -------------------------
eq_gridsize::Int32= 200;  # grid size  
nx::Int32 = eq_gridsize;  # grid size, x axis
ny::Int32 = eq_gridsize;  # grid size, y axis
dx::Float32= 1.0f0;     # grid spacing (arbitrary units)

Dᵤ::Float32 = 20.0f0;   # diffusion rate, Dᵤ(>0)∈ℝ
Dᵥ::Float32= 15.0f0;    # diffusion rate, Dᵥ(>0)∈ℝ

a::Float32 = 0.025f0;   # parameter a(>0)∈ℝ
b::Float32 = 1.5f0;     # parameter b(>0)∈ℝ

dt::Float32 = 0.001f0;  # time step size
nsteps::Int32 = 30000;    # number of time steps

# ---------------- Initial conditions ------------------------
# equilibrium point from linear analysis: (eqpt_u, eqpt_v)
eqpt_u = Float32(a+b);             
eqpt_v = Float32(b/((a+b)^2));

Random.seed!(314)
# Initial U conditions: equilibrium point (u) + random noise
U = fill(eqpt_u,   nx, ny) .+ 0.1f0 .* randn(Float32, nx, ny)

Random.seed!(31415)
# Initial V conditions: equilibrium point (v) + random noise
V = fill(eqpt_v,   nx, ny) .+ 0.1f0 .* randn(Float32, nx, ny)

# ------------------------- simulation  -----------------------
for step in 1:nsteps
    𝓛u = laplacian_neumann_5pt(U, dx)   # from Laplacian.jl
    𝓛v = laplacian_neumann_5pt(V, dx)

    f = a .- U .+ ((U.^2) .* V)
    g = b .- ((U.^2) .* V)

    U .+= dt .* (Dᵤ .* 𝓛u .+ f)
    V .+= dt .* (Dᵥ .* 𝓛v .+ g)
end

# ---------------- Plotting defaults -------------------------
colourgrad = gm7;

default(fontfamily="Computer Modern", size = (900, 800)
            , tickfontsize=12
            , guidefontsize=14
            , titlefontsize=16
            , colorbar_tickfontsize=11)

# -------------- Plot final heatmap ------------------------
plottitleV = L"\textit{v(T)}, " *"       "* "time step ="*" $(nsteps) \n " * "for "*L"[D_{u}, D_{v}, a, b] = " * "[$Dᵤ, $Dᵥ, $a, $b]" * " , world size = $(eq_gridsize)"
pVf = heatmap(V; c =colourgrad,  
             title = plottitleV,
             aspect_ratio = 1,
             colorbar = true,
             axis = nothing,
             xlims = (1, nx), ylims = (1, ny),
             size = (900, 800),
             xticks = false, yticks = false
             , titlelocation=:left
             )
#savefig(pVf, "schnakenberg_V_$(nsteps)_$(Dᵤ)_$(Dᵥ)_$(a)_$(b).png")
