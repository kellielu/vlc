#Installation for VLC language

###Hardware Requirements:
Functioning NVIDIA GPU

###Software Requirements:
OCaml
CUDA Nvidia Toolkit

####For Ubuntu Linux:
    sudo apt-get install ocaml
    sudo dpkg -i cuda-repo-ubuntu1404_7.5-18_amd64.deb 
    sudo apt-get update
    sudo apt-get install cuda

####For MacOS:
Download the CUDA Nvidia Toolkit for Mac from https://developer.nvidia.com/cuda-downloads

If you do not have Homebrew, install it by running the script:

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
Then run:

    brew install ocaml

####For Windows
Download the CUDA Nvidia Toolkit for Windows from https://developer.nvidia.com/cuda-downloads

### Installing and Uninstalling
Change directory on Terminal/Console to PATH/vlc_folder and run:

    make install

To uninstall, run:
    
    make uninstall

# Running VLC
To run VLC after installation, simply run 

    vlc [mode] <source_file>

in the folder where you would like the compiled files to exist.