#import "@preview/polylux:0.4.0": slide as plSlide, toolbox
#import "./cambridge_polylux_theme/lib.typ": cam-blue, cam-dark-blue, cam-light-blue, cam-slate-4, logo, slide
#import "@preview/mannot:0.3.1": *
#import "@preview/fletcher:0.5.8" as fletcher: diagram, edge, node


#logo.update(image("cam-logo-colour-preferred.svg"))

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

  When we model classical diffusion we don't deal with the environment
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
    mark(hat(H)_s, tag: #<system_hamiltonian>, color: #cam-dark-blue)
    +
    mark(hat(H)_e, tag: #<environment_hamiltonian>, color: #cam-dark-blue)
    +
    mark(hat(H)_"int", tag: #<interaction_hamiltonian>, color: #cam-dark-blue)
  $
  #annot(<system_hamiltonian>, pos: bottom + left, dx: -4em, dy: -0.3em, leader-connect: "elbow")[#text(
    font: "Open Sans",
    fill: cam-dark-blue,
  )[System]]
  #annot(<environment_hamiltonian>, pos: top + left, dy: -0.4em, dx: -1em, leader-connect: "elbow")[#text(
    font: "Open Sans",
    fill: cam-dark-blue,
  )[Environment]]
  #annot(<interaction_hamiltonian>, pos: right, dx: 1em)[#text(
    font: "Open Sans",
    fill: cam-dark-blue,
    weight: "regular",
  )[Interaction]]
  #grid(
    columns: (70%, 30%),
    [
      $hat(H)_"int"$ can describe a
      very complex interaction - we can't keep track of these processes explicitly

      We can replace the microscopic description of interactions
      with a stochastic process
    ],
    [
      #box(
        width: 100%,
        height: 5em,
        inset: (top: -4em, left: -3em),
        // outset: -10pt,
        [
          #scale(
            diagram(
              node((0, 0), text("System", fill: cam-slate-4), radius: 3.5em, fill: cam-blue),
              node((1, 1.5), text("Environment", fill: cam-slate-4), radius: 3.5em, fill: cam-blue),
              edge(
                (0, 0),
                (1, 1.5),
                "<|-|>",
                label: text($hat(H)_"int"$, size: 2em),
                stroke: stroke(paint: cam-dark-blue, thickness: 2pt),
              ),
            ),
            40%,
          )
        ],
      )

    ],
  )




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
  The particle is described by the Langevin equation, which corresponds
  to a classical Hamiltonian with a linear interaction
  #grid(
    columns: (35%, 65%),
    column-gutter: 0pt,
    [
      #align(center, diagram(
        node((1.5, 0.25), radius: 1em, fill: cam-dark-blue),
        edge(
          (1.5, 0.25),
          (3, 0.25),
          "-|>",
          stroke: (paint: cam-dark-blue, thickness: 2pt),
          label: $F = -frac(d V, d x)$,
        ),
        edge((0, 0), (3, 1), "-", stroke: cam-dark-blue),
      ))
    ],
    [
      #set text(size: 14pt)
      #align(center, [
        // $
        //   frac(d p, d t) = - frac(d V(x), d x) - frac(1, m) integral_0^t d s frac(alpha(t-s), k_b T) p(s) + F(t)
        // $
        $
          H = frac(p^2, 2m) + U(x) + sum_i (frac(p_i^2, 2) + frac(1, 2) omega_i^2 x_i^2) - mark(x, tag: #<linear_interaction>) sum_i sqrt(gamma_i) x_i
        $  #annot(<linear_interaction>, pos: bottom + left, dy: 16pt, leader-connect: "elbow")[#text(
          font: "Open Sans",
          stroke: cam-dark-blue,
          weight: "regular",
        )[Linear interaction]]])
    ],
  )
])


#slide(type: "light", [
  == A Hamiltonian as a Stochastic Process
  The Hamiltonian only looks stochastic if we use the interaction picture

  #align(center, block(
    fill: none,
    inset: 10pt,
    radius: 5pt,
    below: 15pt,
    [
      #set text(size: 14pt)
      $
        #ket($Psi$)arrow.r.double exp(frac(i t, planck) (hat(H)_s + hat(H)_e)) #ket($Psi$)
        quad
        i planck partial_t#(ket($Psi(t)$)) = hat(H)_"int" ("t")#ket($Psi(t)$)
      $],
  ))

  This removes the explicit contributions
  of $hat(H)_e$ from the hamiltonian.


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
          fill: cam-dark-blue,
          weight: "regular",
        )[Field operator]],
      )
    ],
    image("changing_field_operator.svg", height: 4em),
  ))

])

#slide(type: "light", [
  == The Caldeira Leggett Model

  If we replace each term in the classical Hamiltonian with operators we get the Caldeira-Leggett model.

  We have a single system operator $hat(A) (t) = hat(x) (t)$ and a field operator
  $
    hat(Phi) (t) = sum_i sqrt(gamma_i) hat(x)_i (t)
  $

  We can characterize the environment operators in the same way as before
  $
    #expect($hat(Phi) (t) hat(Phi) (s)$) _e = alpha(t-s) = integral_(-infinity)^infinity d omega frac(planck, omega) gamma(omega) #markrect($n_b (omega)$, tag: <replaces_kbt>, fill: cam-light-blue, stroke: none)exp(i omega (t-s))
  $

])

#slide(type: "light", [
  == Classical Environments interacting with Quantum Systems

  The correllation function $alpha(t-s)$ is the same - but we replace
  $
    frac(k_b T, planck omega) arrow n_b (omega) = (exp(frac(planck omega, k_b T)) - 1)^(-1)
  $
  #grid(
    columns: (75%, 25%),
    [
      Detailed balance is lost if we directly couple a quantum and
      classical system [1,2]

      Classical markovian environments (with $gamma(omega) = frac(2 m lambda omega^2, pi)$) are non markovian at finite temperatures
    ],
    [
      #scale(
        diagram(
          edge((0, 0), (2, 0), "-", stroke: (paint: cam-dark-blue, thickness: 4pt)),
          edge((0, 2), (2, 2), "-", stroke: (paint: cam-dark-blue, thickness: 4pt)),

          edge(
            (0.2, 0),
            (0.2, 2),
            "-|>",
            stroke: cam-dark-blue,
            label: $exp(frac(planck omega, k_b T))$,
            label-angle: left,
          ),
          edge(
            (1.8, 2),
            (1.8, 0),
            "-|>",
            stroke: cam-dark-blue,
            label: $exp(-frac(planck omega, k_b T))$,
            label-angle: right,
          ),
        ),
        80%,
      )
    ],
  )


  #place(
    bottom + left,
    rect(
      fill: none,
      stroke: none,
      inset: 8pt,
      width: 100%,
      [
        #set text(size: 8pt)
        #set par(leading: 0.5em)

        *[1]* Priya V. Parandekar and John C. Tully, “Detailed Balance in Ehrenfest Mixed Quantum-Classical Dynamics,” Journal of Chemical Theory and Computation 2, no. 2 (March 2006): 229–235.

        *[2]* Adolfo Bastida et al., “A Modified Ehrenfest Method That Achieves Boltzmann Quantum State Populations,” Chemical Physics Letters 417, no. 1 (January 9, 2006): 53–57.
      ],
    ),
  )





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
  #place(bottom + center, dy: 1em, [
    #set text(size: 20pt)
    $
      expect(hat(Phi)(x,t)hat(Phi)(x',t'))_e = -frac(1, 4) mark(|x - x'|^2, tag: #<small_delta_x>) alpha(t-t')
    $
    #annot(<small_delta_x>, pos: top, dy: -12pt, dx: 100pt, leader-connect: "elbow")[#text(
      font: "Open Sans",
      fill: cam-dark-blue,
      weight: "regular",
    )[Small width wavepacket]]
  ])
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
    hat(H)_"int" = integral_x hat(rho)_s (x) sum_(k, k') exp(i(q_k - q_(k'))x) tilde(V)(k- k') hat(c)_k^dagger hat(c)_(k')
  $

  From here we can identify the field operators, and characterize the environment

])
#slide(type: "light", [
  == Zeroth Order

  #grid(
    columns: (75%, 25%),
    [
      If we ignore interactions, there is only a single process which contributes

      At low temperatures, scattering only happens in a thin shell around the fermi surface




    ],
    [
      #box(
        width: 100%,
        height: 2em,
        inset: (top: 1em, left: 2em),
        scale(
          diagram(
            node((0, 0)),
            node((0.1, 0.5)),
            node((0.9, 0.5)),
            node((1, 0)),
            edge((0.1, 0.5), (0.9, 0.5), "-|>-", stroke: cam-dark-blue, bend: 60deg),
            edge((0.1, 0.5), (0.9, 0.5), "-<|-", stroke: cam-dark-blue, bend: -60deg),

            edge((0.1, 0.5), (0.9, 0.5), "--|>--", stroke: cam-dark-blue),
            edge((0, 0.1), (0.1, 0.5), "--|>--", stroke: cam-dark-blue),
            edge((0.9, 0.5), (1, 0.1), "--|>--", stroke: cam-dark-blue),
          ),
          200%,
        ),
      )
    ],
  )
  In the small displacement limit $|x - x'| arrow.r 0$ we get ohmic friction
  $
    gamma(omega) = omega^2 frac(2k_f^4m_e^2, (2 pi)^6planck^4) integral d^2 Omega_k d^2 Omega_(k') |V(k_f (k- k'))|^2|k - k'|^2
  $


])

#slide(type: "light", [
  == Second Order
  #box(
    width: 75%,
    [
      Including electron-phonon interactions is more difficult

      We need to make use of perturbation theory to expand in
      orders of the interaction Hamiltonian

      These contributions lead to non ohmic friction - memory is stored as displacement of the lattice
    ],
  )

  Memory effects are likely to be more significant when they occur over timescales similar to the system dynamics


  #place(
    top + center,
    dx: 12em,
    dy: 2.5em,
    stack(
      dir: ttb,
      spacing: 1em,
      diagram(
        node((0, 0)),
        node((0.1, 0.3)),
        node((0.9, 0.3)),
        edge((0.1, 0.3), (0.9, 0.3), "--|>--", stroke: cam-dark-blue),
        edge((0, 0), (0.1, 0.3), "--|>--", stroke: cam-dark-blue),
        edge((0.9, 0.3), (1, 0), "--|>--", stroke: cam-dark-blue),


        node((0.5, 0.5), radius: 0.2em, fill: cam-dark-blue),
        node((0.5, 0.8), radius: 0.2em, fill: cam-dark-blue),

        edge((0.1, 0.3), (0.5, 0.5), "-|>-", stroke: cam-dark-blue),
        edge((0.5, 0.5), (0.9, 0.3), "-|>-", stroke: cam-dark-blue),
        edge((0.5, 0.5), (0.5, 0.8), "~", stroke: cam-dark-blue),
        edge((0.1, 0.3), (0.5, 0.8), "-<|-", stroke: cam-dark-blue, bend: -30deg),
        edge((0.5, 0.8), (0.9, 0.3), "-<|-", stroke: cam-dark-blue, bend: -30deg),
      ),

      diagram(
        node((0, 0)),
        node((0.1, 0.3)),
        node((0.9, 0.3)),
        edge((0.1, 0.3), (0.9, 0.3), "--|>--", stroke: cam-dark-blue),
        edge((0, 0), (0.1, 0.3), "--|>--", stroke: cam-dark-blue),
        edge((0.9, 0.3), (1, 0), "--|>--", stroke: cam-dark-blue),

        node((0.2, 0.7), radius: 0.2em, fill: cam-dark-blue),
        node((0.8, 0.7), radius: 0.2em, fill: cam-dark-blue),

        edge((0.1, 0.3), (0.2, 0.7), "-|>-", stroke: cam-dark-blue, bend: 60deg),
        edge((0.8, 0.7), (0.9, 0.3), "-|>-", stroke: cam-dark-blue, bend: 60deg),
        edge((0.2, 0.7), (0.8, 0.7), "~", stroke: cam-dark-blue),
        edge((0.1, 0.3), (0.2, 0.7), "-<|-", stroke: cam-dark-blue, bend: -60deg),
        edge((0.8, 0.7), (0.9, 0.3), "-<|-", stroke: cam-dark-blue, bend: -60deg),
      ),
    ),
  )






])


#slide(type: "light", [
  == Master Equations and Field Operators

  The characterization of field operators is not just useful for
  comparing environments - they show up in the master equations!

  They also show up as memory in the stochastic Schrodinger equation [1]
  $
    partial_t ket(psi) = -i hat(A) ( mark(phi(t), tag: #<hermitian_process>) + integral_0^t d s mark(G(t-s), tag: #<correllation>) frac(delta, delta phi(s))) ket(psi)
  $

  #annot(<hermitian_process>, pos: bottom + left, dy: 24pt, leader-connect: "elbow")[#text(
    font: "Open Sans",
    fill: cam-dark-blue,
    weight: "regular",
  )[Hermitian random process]]

  #annot(<correllation>, pos: bottom + right, dy: 24pt, leader-connect: "elbow")[#text(
    font: "Open Sans",
    fill: cam-dark-blue,
    weight: "regular",
  )[Correlation Function]]
  #place(
    bottom + left,
    rect(
      fill: none,
      stroke: none,
      inset: 8pt,
      width: 100%,
      [
        #set text(size: 8pt)
        #set par(leading: 0.5em)

        *[1]* L. Diosi and L. Ferialdi, “General Non-Markovian Structure of Gaussian Master and Stochastic Schrodinger Equations” Phys.Rev.Lett. 113 (2014) 200403-(5).
      ],
    ),
  )


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
