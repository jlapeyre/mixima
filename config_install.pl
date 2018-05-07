#######        USER EDIT BELOW              ################
# If the installation script fails to find the share directory, or
# if you want to put the files in a different directory
# then uncomment one of the following lines, set to the appropriate path

# examples for setting the installation direction
# $Install_share_dir = '/usr/share/maxima/5.21.1/share/';
# $Install_share_dir = '/home/joeuser/.maxima';

# The executable files, 'mockmma', 'mixima' etc are installed not to the
# share directory, but to the directory specified below.

$Install_bin_dir = "/usr/local/bin";

# This is not the full path. The path is hardcoded in the script.
# This just chooses between scripts
# Choose either 'gcl' or 'sbcl'
# This is only used for choosing which script is installed to /usr/local/bin
$Lisp_interpreter_for_scripts = 'gcl';

#######        END USER EDIT                ################


