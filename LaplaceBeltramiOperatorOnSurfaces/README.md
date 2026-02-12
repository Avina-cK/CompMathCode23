# Laplace-Beltrami Operator on an arbitary surface
This folder contains a project done for a Master's seminar project.

## Introduction
This project reviews the paper
> Gerhard Dziuk. Finite Elements for the Beltrami operator on arbitrary surfaces, pages 142–155. Springer Berlin Heidelberg, Berlin, Heidelberg, 1988.
An attempt has been made to replicate the numerical results mentioned.

## Subfolder Contents
### include
Contains helper functions and list of required libraries.

### src
Main scripts used to generate meshes and simulate the Poisson problem.

### isomeshes
A few isomeshes (different levels of refinement), generated using Meshing.jl.

### report
Contains the long and short report submitted for the project.

## Potential to-dos
- Generate meshes with other algorithms.
- Define the Laplace-Beltrami operator, via the generation of the Jacbion (as in https://gridap.github.io/Tutorials/stable/pages/t026_poisson_dev_fe/#Computing-the-gradients-of-the-trial-and-test-FE-space-bases-1)
- Manually defining the normal vector

## Caution
The code does appear to "approximately" solve the Poisson problem on a mesh, however, the relative L-2 error, that is supposed to reduce (when the mesh is refined) does not. This implies there is some error in one or more of the following sections: generation of mesh, defining the right hand side function f, calculation of the normal or defining the weak form (via Gridap.jl).
