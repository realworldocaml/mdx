duniverse(1)                   Duniverse Manual                   duniverse(1)



NNAAMMEE
       duniverse - the spice of build life

SSYYNNOOPPSSIISS
       dduunniivveerrssee _C_O_M_M_A_N_D ...

DDEESSCCRRIIPPTTIIOONN
       The dduunniivveerrssee tool provides a convenient interface to bridge the ooppaamm
       package manager with having a local copy of all the source code
       required to build a project using the dduunnee build tool.

       It works by analysing opam package metadata and calculating a set of
       git tags that can be cloned into the local repository into an
       _o_c_a_m_l___m_o_d_u_l_e_s subdirectory. Once the external code has been pulled into
       the repository, a single dduunnee bbuuiilldd command is sufficient to build the
       whole project in a standalone fashion, without opam being required.
       This is a particularly convenient way of publishing CLI tools to users
       who do not need the full power of opam.

       The basic flow of the tool is provided by three git commands:

       $ dduunniivveerrssee ggiitt--lloocckk
           This converts the opam metadata into a set of git remotes, and
           stores the data in _._d_u_n_i_v_e_r_s_e_/_d_u_n_e_._s_x_p.

       $ dduunniivveerrssee ggiitt--ppuullll
           Pulls the vendored code into a _d_u_n_i_v_e_r_s_e_-_o_f_-_m_a_s_t_e_r branch, where
           you can test the project builds.

       $ dduunniivveerrssee ggiitt--mmeerrggee
           Merges the vendor branch into the mainline _m_a_s_t_e_r branch.

       You can access some of the low-level functionality directly via the
       _d_u_n_i_v_e_r_s_e_-_o_p_a_m, _d_u_n_i_v_e_r_s_e_-_l_o_c_k and _d_u_n_i_v_e_r_s_e_-_p_u_l_l commands, but this
       should not be necessary usually.

CCOOMMMMAANNDDSS
       ggiitt--lloocckk
           calculate Dune metadata and git commit the results

       ggiitt--mmeerrggee
           merge vendored libraries into the mainline branch

       ggiitt--ppuullll
           pull vendored libraries and commit them to a branch

       lloocckk
           generate git tags suitable for vendoring from opam metadata

       ooppaamm
           analyse opam metadata to generate a standalone package list

       ppuullll
           fetch the latest archives of the vendored libraries

       ssttaattuuss
           summarise the libraries tracked by the duniverse

CCOOMMMMOONN OOPPTTIIOONNSS
       ----hheellpp[=_F_M_T] (default=auto)
           Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TTEERRMM env var is `dumb' or undefined.

       ----vveerrssiioonn
           Show version information.

EEXXAAMMPPLLEESS
       These commands will vendor the uuttoopp and ccrraammll commands locally:

       $ dduunniivveerrssee ggiitt--lloocckk ----ppiinn llwwtt uuttoopp ccrraammll
       $ dduunniivveerrssee ggiitt--ppuullll
       $ dduunniivveerrssee ggiitt--mmeerrggee

       Also see _h_t_t_p_s_:_/_/_g_i_t_h_u_b_._c_o_m_/_a_v_s_m_/_p_l_a_t_f_o_r_m for an example of a fully
       bootstrapping use of this tool.

SSEEEE AALLSSOO
       dune(1), duniverse-git-lock(1), duniverse-git-merge(1),
       duniverse-git-pull(1), git(1), opam(1)



Duniverse 11VERSION11                                             duniverse(1)
