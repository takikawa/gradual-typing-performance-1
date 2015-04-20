data
====

Ground-truth results from experiments, generated by the `run.rkt` script.

There are two types of files here:
- `*.rktd` raw output from the `run.rkt` script
- `*.graph` module-graph encodings; the `summary.py` script depends on these.
   There should be one `.graph` file matching every `.rktd` file.

By convention, please name `.rktd` files according to the scheme:
    `MODULENAME-DESCRIPTION.rktd`
Where `MODULENAME` is the name of the module, exactly matching a toplevel project directory in this repo, and `DESCRIPTION` is some adjective describing the data file and identifying it from other results for the same project.

Then name all graph files as:
    `MODULENAME.graph`
The `summary.py` script will filter the `-DESCRIPTION` part of `.rktd` filenames to match all data files for one project to a canonical `.graph` file.