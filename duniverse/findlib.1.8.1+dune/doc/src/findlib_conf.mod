<refentry>

<refmeta>
  <refentrytitle>findlib.conf</refentrytitle>
  <manvolnum>5</manvolnum>
  <refmiscinfo>The findlib package manager for OCaml</refmiscinfo>
</refmeta>

<refnamediv id="findlib.conf">
  <refname>findlib.conf</refname>
  <refpurpose>[Configuration of findlib/ocamlfind]</refpurpose>
</refnamediv>


<refsect1>
<title>GENERAL</title>

<para>
There are three possibilities to configure the findlib library:

<variablelist>
<varlistentry>
  <term>Build time:</term>
  <listitem><para>
    Before findlib is compiled, a "configure" script is invoked to figure
    out the settings that are most likely to work on the system. Most
    settings are simply entered into text files and can easily be changed
    after installation. The following properties cannot be changed later
    because they are compiled into the resulting binaries:

    <itemizedlist>
    <listitem>
    <para>
    The default location of the configuration file findlib.conf. However,
    you can set a different location by the environment variable
    <literal>OCAMLFIND_CONF</literal>.
    </para>
    </listitem>

    <listitem>
    <para>
    Whether the installed O'Caml version supports autolinking or not.
    </para>
    </listitem>

   </itemizedlist>
  </para>
  </listitem>
</varlistentry>

<varlistentry>
  <term>Configuration file findlib.conf:</term>
  <listitem><para>
    An initial version of this file is generated by the configure script,
    but you are free to modify it later. Most important, this file
    contains the system-wide search path used to look up packages, and the
    default location where to install new packages.
  </para>
  <para>
    All files with the suffix ".conf" found in the directory
    findlib.conf.d are also scanned for parameters.
  </para>
  </listitem>
</varlistentry>

<varlistentry>
  <term>Environment variables:</term>
  <listitem><para>
    The settings of findlib.conf can be overridden by environment variables.
  </para>
  </listitem>
</varlistentry>
</variablelist>

Last but not least, several settings can also be passed as
command-line options, or by invoking the function
<literal>Findlib.init</literal>. 
</para>
</refsect1>


<!-- ********************************************************************** -->


<refsect1>
<title>findlib.conf</title>

<para>
The directory containing findlib.conf is determined at build time (by
running the configure script), the fallback default is
<literal>/usr/local/etc</literal>. You can set a different location by
changing the environment variable <literal>OCAMLFIND_CONF</literal>
which must contain the absolute path of findlib.conf.
</para>

<para>
The file has the same syntax as <link
linkend="META"><literal>META</literal></link>, i.e. it consists of a
number of lines with the format

<programlisting>
<replaceable>variable</replaceable> = "<replaceable>value</replaceable>"
</programlisting>
</para>

<para>Here is the list of allowed variables:

<variablelist>
<varlistentry>
  <term><literal>path</literal></term>
  <listitem><para>
    The search path for META files/package directories. The variable
    enumerates directories which are separated by colons (Windows:
    semicolons), and these directories are tried in turn to find a certain
    package. More exactly, if d is such a directory and p the searched
    package, the search algorithm will first check whether d/p/META
    exists. In this case, this META file is taken, and d/p is the package
    directory. Second, the algorithm tries d/META.p, but the package
    directory must be specified in this META.p file by a
    <literal>directory</literal> directive.
  </para>

  <para>
    Note that the first found META file is taken, so the order of the
    directories in the search path counts.
  </para>

  <para>
    This variable is required.
  </para>

  <para>
  Example:

  <programlisting>
path = "/usr/local/lib/ocaml/site-lib:/usr/lib/ocaml/site-lib"
</programlisting>
  </para>
  </listitem>
</varlistentry>
</variablelist>


<variablelist>
<varlistentry>
  <term><literal>destdir</literal></term>
  <listitem><para>
    This variable determines the location where <literal>ocamlfind
    install</literal> puts the packages by default: If d is this
    directory, and p the package to install, a new subdirectory d/p will
    be created containing all the files of the package.
  </para>

  <para>Example:
  <programlisting>
destdir = "/usr/local/lib/ocaml/site-lib"
</programlisting>
  </para>

  <para>
    This variable is required.
  </para>
  </listitem>
</varlistentry>
</variablelist>


<variablelist>
<varlistentry>
  <term><literal>metadir</literal></term>
  <listitem><para>
    If set, the command <literal>ocamlfind install</literal> will put the
    META files of packages into this directory (files are named META.p
    where p=package name); otherwise the META files are put into the
    package directories like any other file.
  </para>

  <para>Example:
  <programlisting>
metadir = "/var/lib/findlib/metaregistry"
</programlisting>
  </para>

  <para>
    This variable is optional. It is not used by default.
  </para>
  </listitem>
</varlistentry>
</variablelist>


<variablelist>
<varlistentry>
  <term><literal>ocamlc</literal>,
        <literal>ocamlopt</literal>,
        <literal>ocamlcp</literal>,
        <literal>ocamlmktop</literal>,
	<literal>ocamldoc</literal>,
	<literal>ocamldep</literal>,
	<literal>ocamlbrowser</literal>
  </term>
  <listitem><para>
    If you want to call other executables than "ocamlc", "ocamlopt",
    "ocamlcp", "ocamlmktop", "ocamldoc", "ocamldep", and
    "ocamlbrowser", you can
    set the names of
    the executables here. The command <literal>ocamlfind</literal> looks
    into these four variables to determine the names of the compilers to
    call.
  </para>

  <para>Example:
  <programlisting>
ocamlc     = "ocamlc.opt"
ocamlopt   = "ocamlopt.opt"
ocamlcp    = "ocamlcp.opt"
ocamlmktop = "ocamlmktop.opt"
</programlisting>
  </para>

  <para>
    This variable is optional. It is not used by default.
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>stdlib</literal></term>
  <listitem><para>
    This variable determines the location of the standard library. This must
    be the same directory for which the O'Caml compilers are configured.
  </para>

  <para>
    This variable is optional. It is not recommend to set this variable
    unless you know what you are doing!
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>ldconf</literal></term>
  <listitem><para>
    This variable determines the location of the ld.conf file. This must
    be the same file the O'Caml compilers read in; it is updated by 
    ocamlfind when installing and removing packages. You can set this
    variable to the special value "<literal>ignore</literal>" to disable
    the automatic modification of the ld.conf file.
  </para>

  <para>
    If not set, the ld.conf file is assumed to reside in the O'Caml
    standard library directory.
  </para>

  <para>
    This variable is optional. It is not recommended to set this variable
    unless you know what you are doing!
  </para>
  </listitem>
</varlistentry>
</variablelist>
</para>

<para>
Toolchains: It is possible to have variants of the original configuration.
These variants are called "toolchains" because they are intended to
select different compilers, e.g. patched compilers. In order to
set a variable for a certain toolchain, use the syntax

<programlisting>
<replaceable>variable</replaceable>(<replaceable>toolchain</replaceable>) = "<replaceable>value</replaceable>"
</programlisting>

For example:

<programlisting>
ocamlc(mypatch) = "ocamlc-mypatch"
</programlisting>

When the toolchain "mypatch" is selected, this compiler will be used instead
of the standard one.</para>

<para>In order to switch to a certain toolchain, use the -toolchain
option of <link linkend="ocamlfind">ocamlfind</link>.</para>

</refsect1>



<!-- ********************************************************************** -->


<refsect1>
<title>Environment</title>

<para>
A number of environment variables modifies the behaviour of
findlib/ocamlfind:

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_CONF</literal></term>
  <listitem><para>
    This variable overrides the location of the configuration file
    findlib.conf. It must contain the absolute path name of this file.
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_TOOLCHAIN</literal></term>
  <listitem><para>
    This variable sets the currently selected toolchain when
    a <literal>-toolchain</literal> option is not passed
    on the command line.
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLPATH</literal></term>
  <listitem><para>
    This variable may contain an additional search path for package
    directories. It is treated as if the directories were prepended to
    the configuration variable <literal>path</literal>.
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_DESTDIR</literal></term>
  <listitem><para>
    This variable overrides the configuration variable
    <literal>destdir</literal>. 
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_METADIR</literal></term>
  <listitem><para>
    This variable overrides the configuration variable
    <literal>metadir</literal>. 
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_COMMANDS</literal></term> <listitem><para>
  This variable overrides the configuration variables
  <literal>ocamlc</literal>, <literal>ocamlopt</literal>,
  <literal>ocamlcp</literal>, <literal>ocamlmktop</literal>,
  <literal>ocamldoc</literal>, <literal>ocamldep</literal>, and/or
  <literal>ocamlbrowser</literal>. 
  Its value must conform to the syntax

    <programlisting>
ocamlc=<replaceable>name</replaceable> ocamlopt=<replaceable>name</replaceable> ocamlcp=<replaceable>name</replaceable> ocamlmktop=<replaceable>name</replaceable> ocamldoc=<replaceable>name</replaceable> ocamldep=<replaceable>name</replaceable> ocamlbrowser=<replaceable>name</replaceable>
</programlisting>
  </para>

  <para>Example:
    <programlisting>
ocamlc=ocamlc-3.00 ocamlopt=ocamlopt-3.00 ocamlcp=ocamlcp-3.00 ocamlmktop=ocamlmktop-3.00
</programlisting>
  </para>

  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>CAMLLIB</literal> or <literal>OCAMLLIB</literal></term>
  <listitem><para>
    This variable overrides the configuration variable
    <literal>stdlib</literal>. 
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_LDCONF</literal></term>
  <listitem><para>
    This variable overrides the configuration variable
    <literal>ldconf</literal>. 
  </para>
  </listitem>
</varlistentry>
</variablelist>

<variablelist>
<varlistentry>
  <term><literal>OCAMLFIND_IGNORE_DUPS_IN</literal></term>
  <listitem><para>
    This variable instructs findlib not to emit warnings that packages
    or module occur several times. The variable must be set to the
    directory where the packages reside that are to be ignored for this
    warning.
  </para>
  </listitem>
</varlistentry>
</variablelist>


</para>
</refsect1>


</refentry>