# ─────────────────────────── Laplacian (Neumann BCs) ─────────────────────────
# ----------- 5 point stencil -------------------------------------------------
function laplacian_neumann_5pt(A::AbstractMatrix{T}, dx::Real = 1.0) where {T<:Real}
    """ 5-point Laplacian stencil with Neumann boundary conditions"""
    nx, ny = size(A)
    L      = similar(A)
    @inbounds for j in 1:ny, i in 1:nx
        ip = (i == nx) ? nx - 1 : i + 1
        im = (i == 1)  ? 2      : i - 1
        jp = (j == ny) ? ny - 1 : j + 1
        jm = (j == 1)  ? 2      : j - 1

        L[i, j] = (A[im, j] + A[ip, j] + A[i, jm] + A[i, jp] - 4A[i, j]) / dx^2
    end
    return L
end

# 5-point stencil from https://docs.sciml.ai/DiffEqDocs/stable/examples/beeler_reuter/
function laplacian_neumann_5pt_2(u)
    n1, n2 = size(u)
    𝓛      = similar(u)
    # internal nodes
    for j in 2:(n2 - 1)
        for i in 2:(n1 - 1)
            @inbounds 𝓛[i, j] = u[i + 1, j] + u[i - 1, j] + u[i, j + 1] + u[i, j - 1] -
                                 4 * u[i, j]
        end
    end

    # left/right edges
    for i in 2:(n1 - 1)
        @inbounds 𝓛[i, 1] = u[i + 1, 1] + u[i - 1, 1] + 2 * u[i, 2] - 4 * u[i, 1]
        @inbounds 𝓛[i, n2] = u[i + 1, n2] + u[i - 1, n2] + 2 * u[i, n2 - 1] - 4 * u[i, n2]
    end

    # top/bottom edges
    for j in 2:(n2 - 1)
        @inbounds 𝓛[1, j] = u[1, j + 1] + u[1, j - 1] + 2 * u[2, j] - 4 * u[1, j]
        @inbounds 𝓛[n1, j] = u[n1, j + 1] + u[n1, j - 1] + 2 * u[n1 - 1, j] - 4 * u[n1, j]
    end

    # corners
    @inbounds 𝓛[1, 1] = 2 * (u[2, 1] + u[1, 2]) - 4 * u[1, 1]
    @inbounds 𝓛[n1, 1] = 2 * (u[n1 - 1, 1] + u[n1, 2]) - 4 * u[n1, 1]
    @inbounds 𝓛[1, n2] = 2 * (u[2, n2] + u[1, n2 - 1]) - 4 * u[1, n2]
    @inbounds 𝓛[n1, n2] = 2 * (u[n1 - 1, n2] + u[n1, n2 - 1]) - 4 * u[n1, n2]
    return 𝓛
end

# ----------- 9 point stencil -------------------------------------------------
function laplacian_neumann_9pt(A::AbstractMatrix{T}, dx::Real = 1.0) where {T<:Real}
    """ 9-point Laplacian stencil with Neumann boundary conditions"""
    nx, ny = size(A)
    L = similar(A)

    @inbounds for j in 1:ny, i in 1:nx
        ip = (i == nx) ? nx - 1 : i + 1
        im = (i == 1)  ? 2      : i - 1
        jp = (j == ny) ? ny - 1 : j + 1
        jm = (j == 1)  ? 2      : j - 1

        L[i, j] = (
            4 * (A[im, j] + A[ip, j] + A[i, jm] + A[i, jp]) +
            (A[im, jm] + A[im, jp] + A[ip, jm] + A[ip, jp]) -
            20 * A[i, j]
        ) / (6 * dx^2)
    end

    return L
end

