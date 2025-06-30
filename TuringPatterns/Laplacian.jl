# ─────────────────────────── Laplacian (Neumann BCs) ─────────────────────────
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
