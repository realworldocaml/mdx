# duniverse -- the spice of OCaml build life

This is an experimental vendoring system for Dune.  Not for public consumption
just yet.

Please see the manual pages for an explanation of how to use the tool.  Contact
`@avsm` if you have any queries.


# Manual Pages

## duniverse

```
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

```

## duniverse-git-lock

```
duniverse-git-lock(1)          Duniverse Manual          duniverse-git-lock(1)



NNAAMMEE
       duniverse-git-lock - calculate Dune metadata and git commit the results

SSYYNNOOPPSSIISS
       dduunniivveerrssee ggiitt--lloocckk [_O_P_T_I_O_N]... [_P_A_C_K_A_G_E_S]...

DDEESSCCRRIIPPTTIIOONN
       This initiaises a Git repository with the vendoring metadata for Dune,
       and commits the results to the current branch. It runs

AARRGGUUMMEENNTTSS
       _P_A_C_K_A_G_E_S
           opam packages to calculate duniverse for. If not supplied, any
           local opam metadata files are used as the default package list.

OOPPTTIIOONNSS
       --bb _B_R_A_N_C_H (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the _m_a_s_t_e_r branch is used.

       ----hheellpp[=_F_M_T] (default=auto)
           Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TTEERRMM env var is `dumb' or undefined.

       ----ooppaamm--rreemmoottee=_O_P_A_M___R_E_M_O_T_E
           Extra opam remotes to add when resolving package names. Repeat this
           flag multiple times for more than one remote.

       --pp _P_I_N, ----ppiinn=_P_I_N
           Packages to pin for the latest opam metadata and source. You can
           separate the package name and a url and a remote branch via commas
           to specify a manual url (e.g.
           _m_i_r_a_g_e_,_g_i_t_:_/_/_g_i_t_h_u_b_._c_o_m_/_a_v_s_m_/_m_i_r_a_g_e_,_f_i_x_m_e). If a url is not
           specified then the _-_-_d_e_v pin is used. If a branch is not specified
           then the default remote branch is used. Repeat this flag multiple
           times for more than one exclusion.

       --rr _T_A_R_G_E_T___R_E_P_O, --------rreeppoo=_T_A_R_G_E_T___R_E_P_O (absent=.)
           Path to Git repository to store vendored code in.

       --ss _O_P_A_M___S_W_I_T_C_H, ----ooppaamm--sswwiittcchh=_O_P_A_M___S_W_I_T_C_H (absent=ocaml-system)
           Name of the OCaml compiler to use to resolve opam packages. A local
           switch is created in _._d_u_n_i_v_e_r_s_e where pins and packages can be
           tracked independently of your main opam switch. This defaults to
           _o_c_a_m_l_-_s_y_s_t_e_m, but you can use this flag to supply a more specific
           version such as ooccaammll..44..0066..11.

       ----vveerrssiioonn
           Show version information.

       --xx _E_X_C_L_U_D_E, ----eexxcclluuddee=_E_X_C_L_U_D_E
           Packages to exclude from the output list. You can use this to
           remove the root packages so they are not duplicated in the vendor
           directory. Repeat this flag multiple times for more than one
           exclusion.

CCOOMMMMOONN OOPPTTIIOONNSS
       ----ccoolloorr=_W_H_E_N (absent=auto)
           Colorize the output. _W_H_E_N must be one of `auto', `always' or
           `never'.

       --qq, ----qquuiieett
           Be quiet. Takes over --vv and ----vveerrbboossiittyy.

       --vv, ----vveerrbboossee
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       ----vveerrbboossiittyy=_L_E_V_E_L (absent=warning)
           Be more or less verbose. _L_E_V_E_L must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over --vv.

EEXXIITT SSTTAATTUUSS
       ggiitt--lloocckk exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SSEEEE AALLSSOO
       duniverse-git-merge(1), duniverse-git-pull(1)



Duniverse 11VERSION11                                    duniverse-git-lock(1)

```

## duniverse-git-pull

```
duniverse-git-pull(1)          Duniverse Manual          duniverse-git-pull(1)



NNAAMMEE
       duniverse-git-pull - pull vendored libraries and commit them to a
       branch

SSYYNNOOPPSSIISS
       dduunniivveerrssee ggiitt--ppuullll [_O_P_T_I_O_N]...

DDEESSCCRRIIPPTTIIOONN
       This command wraps _d_u_n_i_v_e_r_s_e _p_u_l_l with a workflow that stores the
       vendored libraries in a git branch called dduunniivveerrssee--ooff--<<bbrraanncchh>>. By
       storing them in a separate branch, you can then test that they work
       (e.g. with continuous integration) and then integrate them either in
       your mmaasstteerr branch or in a separate release branch.

       Once this command is complete, you can merge the results with _d_u_n_i_v_e_r_s_e
       _g_i_t_-_m_e_r_g_e.

OOPPTTIIOONNSS
       --bb _B_R_A_N_C_H (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the _m_a_s_t_e_r branch is used.

       ----hheellpp[=_F_M_T] (default=auto)
           Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TTEERRMM env var is `dumb' or undefined.

       --rr _T_A_R_G_E_T___R_E_P_O, --------rreeppoo=_T_A_R_G_E_T___R_E_P_O (absent=.)
           Path to Git repository to store vendored code in.

       ----vveerrssiioonn
           Show version information.

CCOOMMMMOONN OOPPTTIIOONNSS
       ----ccoolloorr=_W_H_E_N (absent=auto)
           Colorize the output. _W_H_E_N must be one of `auto', `always' or
           `never'.

       --qq, ----qquuiieett
           Be quiet. Takes over --vv and ----vveerrbboossiittyy.

       --vv, ----vveerrbboossee
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       ----vveerrbboossiittyy=_L_E_V_E_L (absent=warning)
           Be more or less verbose. _L_E_V_E_L must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over --vv.

EEXXIITT SSTTAATTUUSS
       ggiitt--ppuullll exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).

SSEEEE AALLSSOO
       duniverse-git-lock(1), duniverse-git-merge(1)



Duniverse 11VERSION11                                    duniverse-git-pull(1)

```

## duniverse-status

```
duniverse-status(1)            Duniverse Manual            duniverse-status(1)



NNAAMMEE
       duniverse-status - summarise the libraries tracked by the duniverse

SSYYNNOOPPSSIISS
       dduunniivveerrssee ssttaattuuss [_O_P_T_I_O_N]...

DDEESSCCRRIIPPTTIIOONN
       This command looks at the various metadata files in the ..dduunniivveerrssee
       directory and prints them out in a human-readable format.

OOPPTTIIOONNSS
       --bb _B_R_A_N_C_H (absent=master)
           Branch that represents the working tree of the source code. If not
           supplied, the _m_a_s_t_e_r branch is used.

       ----hheellpp[=_F_M_T] (default=auto)
           Show this help in format _F_M_T. The value _F_M_T must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TTEERRMM env var is `dumb' or undefined.

       --rr _T_A_R_G_E_T___R_E_P_O, --------rreeppoo=_T_A_R_G_E_T___R_E_P_O (absent=.)
           Path to Git repository to store vendored code in.

       ----vveerrssiioonn
           Show version information.

CCOOMMMMOONN OOPPTTIIOONNSS
       ----ccoolloorr=_W_H_E_N (absent=auto)
           Colorize the output. _W_H_E_N must be one of `auto', `always' or
           `never'.

       --qq, ----qquuiieett
           Be quiet. Takes over --vv and ----vveerrbboossiittyy.

       --vv, ----vveerrbboossee
           Increase verbosity. Repeatable, but more than twice does not bring
           more.

       ----vveerrbboossiittyy=_L_E_V_E_L (absent=warning)
           Be more or less verbose. _L_E_V_E_L must be one of `quiet', `error',
           `warning', `info' or `debug'. Takes over --vv.

EEXXIITT SSTTAATTUUSS
       ssttaattuuss exits with the following status:

       0   on success.

       124 on command line parsing errors.

       125 on unexpected internal errors (bugs).



Duniverse 11VERSION11                                      duniverse-status(1)

```

