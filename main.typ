#import "@preview/polylux:0.4.0": slide as plSlide, toolbox
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

  Matt Ord
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
    mark(hat(H)_e, tag: #<environment_hamiltonian>, color: camDarkBlue)
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
  #annot(<environment_hamiltonian>, pos: top + left, dy: -0.4em, dx: -1em, leader-connect: "elbow")[#text(
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
  == The Classical Environment
  In the Classical model, a particle experiences a stochastic force $F(t)$
  #[
    #set text(size: 16pt)
    $
      #expect($F(t) F(s)$) _e = alpha(t-s) = integral_(-infinity)^infinity d omega frac(planck, omega) gamma(omega) frac(k_b T, planck omega) exp(i omega (t-s))
    $
  ]
  // TODO: plot demonstarting force from linear V(x)
  The particle is described by the Langevin equation, or a Hamiltonian
  #align(center, block(
    fill: camLightBlue,
    inset: 10pt,
    radius: 5pt,
    below: 15pt,
    [
      #set text(size: 14pt)
      $
        frac(d p, d t) = - frac(d V(x), d x) - frac(1, m) integral_0^t d s frac(alpha(t-s), k_b T) p(s) + F(t)
      $
      $
        H = frac(p^2, 2m) + U(x) + sum_i (frac(p_i^2, 2) + frac(1, 2) omega_i^2 x_i^2) - mark(x, tag: #<linear_interaction>) sum_i sqrt(gamma_i) x_i
      $  #annot(<linear_interaction>, pos: top + right, dy: -16pt, dx: 4em, leader-connect: "elbow")[#text(
        font: "Open Sans",
        stroke: camDarkBlue,
        weight: "regular",
      )[Linear interaction]]
    ],
  ))
])


#slide(type: "light", [
  == A Hamiltonian as a Stochastic Process
  So how do we write down a Hamiltonian that looks stochastic?

  We need to use the interaction picture

  #align(center, block(
    fill: camLightBlue,
    inset: 10pt,
    radius: 5pt,
    below: 15pt,
    [
      #set text(size: 14pt)
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
      #set text(size: 16pt)
      #stack(
        dir: ttb,
        spacing: 1em,
        v(0.1em),
        [
          $
            hat(H) = sum_i hat(A)_i (t) mark(hat(Phi)_i (t), tag: #<field_operator>)
          $
        ],
        [
          $
            expect(hat(Phi)_i (t) hat(Phi)_j (s))_e = G_(i,j) (t-s)
          $
        ],
        annot(<field_operator>, pos: right, dy: -24pt)[#text(
          font: "Open Sans",
          stroke: camDarkBlue,
          weight: "regular",
        )[Field operator]],
      )
    ],
    image("changing_field_operator.svg", height: 4em),
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
  == The Caldeira Leggett Model

  If we replace each term in this Hamiltonian with operators we get the Caldeira-Leggett model.

  We have a single system operator $hat(A) (t) = hat(x) (t)$ and a field operator
  $
    hat(Phi) (t) = sum_i sqrt(gamma_i) hat(x)_i (t)
  $

  We can characterize the environment operators in the same way as before
  $
    #expect($hat(Phi) (t) hat(Phi) (s)$) = alpha(t-s) = integral_(-infinity)^infinity d omega frac(planck, omega) gamma(omega) n_b (omega)exp(i omega (t-s))
  $
  // # TODO: Highlight difference between classical and quantum alpha

])

#slide(type: "light", [
  == Classical Environments interacting with Quantum Systems

  TODO: issues with naive approach of converting classical to quantum

])

#slide(type: "light", [
  == Comparing Environments

  To compare different models we write $hat(H)_"int" (t)$ in a consistent way
  #[
    #set text(size: 20pt)
    $
      hat(H)_"int" (t) = integral d x hat(rho)_s (x, t) hat(Phi) (x, t)
      quad
      hat(Phi) (x, t) = hat(V)(x, t) - expect(hat(V)(x,t))_e
    $
  ]
  We can then compare any scattering interaction using $G(x,x', t, t')$
  #[
    #set text(size: 20pt)
    $
      expect(hat(Phi)(x,t)hat(Phi)(x',t'))_e = G(x,x', t, t')
    $
  ]
  For the linear force model we have
  #[
    #set text(size: 20pt)
    $
      expect(hat(Phi)(x,t)hat(Phi)(x',t'))_e = -frac(1, 4) |x - x'|^2 alpha(t-t')
    $
  ]
  //  TODO: annotate and mention small delta x expansion

])

#slide(type: "light", [
  == A Realistic Environment Model

  The Frolich Hamiltonian describes interactions on the surface
  $
    hat(H) = sum_k epsilon_k hat(c)_k^dagger hat(c)_k + sum_q planck omega_q hat(b)_q^dagger hat(b)_q + sum_(k, q) M_(k, q) (hat(b)_(-q)^dagger + hat(b)_q) hat(c)_(k+q)^dagger hat(c)_k
  $
  // TODO: Annotate terms
  The electrons will also scatter off an adsorbate
  $
    hat(H)_"int" = sum_(k, k') V_(k, k') hat(c)_k^dagger hat(c)_(k') hat(X)
  $

  Write down the Frolich Hamiltonian
  derive an expression for alpha
  To simulate this we need Quantum Field theory!!!

])
#slide(type: "light", [
  == Zeroth Order

  #grid(
    columns: (auto, auto),
    [
      If we ignore interactions, there is only a single process which contributes

      At low temperatures, scattering only happens in a thin shell around the fermi surface

      If we take $|x - x'| arrow.r 0$ we get the classical spectrum
      $
        gamma(omega) = omega^2 frac(2k_f^2m_e^2, (2 pi)^6planck^4) integral d^2 Omega_k d^2 Omega_(k') |V(k- k')|^2|k - k'|^2
      $


    ],
    [
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
      // TODO: plot fermi sphere with arrows
    ],
  )



])

#slide(type: "light", [
  == Second Order

  Including electron-phonon interactions is much more difficult

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

  To do this, we will need to use Quantum Field Theory!




])


#slide(type: "light", [
  == Master Equations and Field Operators

  These characteristic functions show up in the master equations ...




])
#slide(type: "alt1", [
  == Conclusion

  System - environment interactions can be treated as stochastic processes

  #grid(
    columns: (50%, 50%),
    [
      - We characterize classical interactions using a force
      - We characterize quantum environments using field operators
      - The spectrum $gamma(omega)$ is a temperature independent property of
        the environment
    ],
    [
      - Taking the small distance limit lets us calculate a classical
        spectrum
      - The Frolich Hamiltonian, which describes a realistic environment,
        is ohmic if we ignore interactions
      - Quantum field theory gives us a way to expand
        the interactions to different orders
    ],
  )




])
