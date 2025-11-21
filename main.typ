#import "@preview/polylux:0.4.0": toolbox
#import "./cambridge_polylux_theme/lib.typ": camDarkBlue, camLightBlue, slide
#import "@preview/mannot:0.3.1": *
#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node



#set page(
  paper: "presentation-16-9",
  footer: align(right, text(size: .8em, toolbox.slide-number)),
  margin: (bottom: 2em, rest: 1em),
)


#let bra(expr) = [$#math.chevron.l expr|$]
#let ket(expr) = [$|expr#math.chevron.r$]
#let expect(expr) = [$#math.chevron.l expr#math.chevron.r$]



#slide(type: "title", [
  #set align(horizon)


  = Characterizing a Quantum Environment

  Matthew Ord
])

#slide[
  = Outline

  When we model diffusion we don't deal with the environment
  directly - we treat interactions as a random process

  - Classically, these interactions appear as a stochastic
    force and deterministic damping felt by the particle
  - Can we characterize the effect of a quantum environment,
    and what effect does it have on the system?
  - What does the process of characterizing a real environment look like,
    for this we will need to use field theory
  - How does this relate to the "Quantum Master Equation" description

]

#slide(type: "light", [
  == An Open System

  A general quantum system is described by a Hamiltonian $hat(H)$ which can be split into three parts
  $
    hat(H) =
    mark(hat(H)_s, tag: #<system_hamiltonian>, color: camDarkBlue)
    +
    mark(hat(H)_e, tag: #<environemnt_hamiltonian>, color: camDarkBlue)
    +
    mark(hat(H)_"int", tag: #<interaction_hamiltonian>, color: camDarkBlue)
  $
  The interaction described by $hat(H)_"int"$ is in general
  very complex - we can't keep track of these processes explicitly

  The core idea of the Langevin equation is that we can replace the microscopic
  description of the environment with a stochastic force


  #annot(<system_hamiltonian>, pos: bottom + left, dx: -4em, dy: -0.3em, leader-connect: "elbow")[#text(
    font: "Open Sans",
    stroke: camDarkBlue,
  )[System]]
  #annot(<environemnt_hamiltonian>, pos: top + left, dy: -0.4em, dx: -1em, leader-connect: "elbow")[#text(
    font: "Open Sans",
    stroke: camDarkBlue,
  )[Environment]]
  #annot(<interaction_hamiltonian>, pos: right, dx: 1em)[#text(
    font: "Open Sans",
    stroke: camDarkBlue,
    weight: "regular",
  )[Interaction]]

])


#slide(type: "light", [
  == A Stochastic Description
  So how do we describe a quantum environment as a stochastic process?

  We need to use the interaction picture

  #align(center, block(
    fill: camLightBlue,
    inset: 10pt,
    radius: 5pt,
    below: 15pt,
    [
      $
        hat(H) arrow.r.double hat(H)_"int" ("t")
        quad
        #ket($Psi$) arrow.r.double exp(frac(i t, planck) (hat(H)_s + hat(H)_e)) #ket($Psi$)
      $],
  ))


  We can write the remaining Hamiltonian as a sum of field operators $hat(Phi)_i (t)$
  #align(center, stack(
    dir: ltr,
    spacing: 5em,
    [
      #v(0.8em)
      $
        hat(H) = sum_i hat(A)_i (t) markrect(hat(Phi)_i (t), tag: #<field_operator>, fill: camLightBlue, stroke: #none, outset: #(top: 0.3em, bottom: 0.3em))
      $],
    image("changing_field_operator.svg", height: 3em),
  ))
  // #stack(
  //   dir: ltr,
  //   spacing: 10em,
  //   [$
  //     hat(H) = sum_i hat(A)_i (t) mark(hat(Phi)_i (t), tag: #<field_operator>)
  //   $],
  //   image("changing_field_operator.svg", height: 3em),
  // )
  // #annot(<field_operator>, pos: top + right)[#text(
  //   font: "Open Sans",
  //   stroke: camDarkBlue,
  //   weight: "regular",
  // )[Field operator]]
  // #image("changing_field_operator.svg", height: 8em)
  // TODO: Plot of random process
  // TODO: Mention of gaussian process

])

#slide(type: "light", [
  == The Classical Environment

  As an example, we can see how this works for the classical environment
  $
    H = frac(p^2, 2m) + U(x) + sum_i (frac(p_i^2, 2) + frac(1, 2) omega_i^2 x_i^2) - markrect(x, tag: #<field_operator>, fill: camLightBlue, stroke: #none, outset: #0.3em) sum_i sqrt(gamma_i) x_i
  $
  $
    frac(d p, d t) = - frac(d V(x), d x) - frac(1, m) integral_0^t d s frac(alpha(t-s), k_b T) p(s) + F(t)
  $

  The particle experiences a stochastic force $F(t)$ characterized by
  $
    #expect($F(t) F(s)$) = alpha(t-s)
  $

])

#slide(type: "light", [
  == Comparing Environments

  We can write any interaction in position basis

  We use this to compare between models

  Classically we have

  We can take the small delta x limit to identify the classical force for any model!

  If we look closely, the ohmic environment we get fr Langevin is not
  the same as quantum case due to equi partition!

  We use gamma omega to compare the same environment at different temperatures

  this is an important point - no real environment is non-Markovian, this
  is only possible to ignore in the classical case.

  Much of the complexity in the CL model is dealing with this problem.

])

#slide(type: "light", [
  == A Realistic Environment Model

  Write down the Frolich Hamiltonian
  derive an expression for alpha
  To simulate this we need Quantum Field theory!!!

])
#slide(type: "light", [
  == Zeroth Order


  #diagram(
    node((0, 0), radius: 0.2em, fill: camDarkBlue),
    edge((0, 0), (0, 0), bend: -170deg, stroke: camDarkBlue),
    node((0.8, 0), radius: 0.2em, fill: camDarkBlue),
    edge((0.8, 0), (0.8, 0), bend: -170deg, stroke: camDarkBlue),
  )

  #diagram(
    node((0, 0)),
    edge((0, 0), (1, 0), "->-", stroke: camDarkBlue),
    node((1, 0)),
    node((0, 0.3)),
    edge((0, 0.3), (1, 0.3), "-<-", stroke: camDarkBlue),
    node((1, 0.3)),
  )


  TODO: plot of zeroth order Feynman diagrams

  Without using the tricks from QFT we can however
  Still get zeroth order non-interacting environment
  contribution


  scattering only happens in a thin shell around the fermi surface - and we can use
  this to simplify
])
#slide(type: "light", [
  == Scattering at Low temperatures

  at "Low" temperatures scattering only happens in a thin shell around the fermi surface - and we can use
  this to simplify

  TODO: picture of fermi sphere with arrows
])
#slide(type: "light", [
  == Second Order

  #stack(
    dir: ltr,
    spacing: 1em,
    diagram(
      node((0.5, 0.2), radius: 0.2em, fill: camDarkBlue),
      node((0.5, 0.8), radius: 0.2em, fill: camDarkBlue),
      edge((0, 0), (0.5, 0.2), "->-", stroke: camDarkBlue),
      edge((0.5, 0.2), (1, 0), "->-", stroke: camDarkBlue),
      edge((0.5, 0.2), (0.5, 0.8), "~", stroke: camDarkBlue),
      edge((0, 1), (0.5, 0.8), "-<-", stroke: camDarkBlue),
      edge((0.5, 0.8), (1, 1), "-<-", stroke: camDarkBlue),
    ),

    diagram(
      node((0.2, 0.5), radius: 0.2em, fill: camDarkBlue),
      node((0.8, 0.5), radius: 0.2em, fill: camDarkBlue),
      edge((0, 0), (0.2, 0.5), "->-", stroke: camDarkBlue),
      edge((0.8, 0.5), (1, 0), "->-", stroke: camDarkBlue),
      edge((0.2, 0.5), (0.8, 0.5), "~", stroke: camDarkBlue),
      edge((0, 1), (0.2, 0.5), "-<-", stroke: camDarkBlue),
      edge((0.8, 0.5), (1, 1), "-<-", stroke: camDarkBlue),
    ),
  )




])
