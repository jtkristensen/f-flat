# Die on first error.
set -e

base_dir="$(dirname "$0")"
fflat="$base_dir/../bin/fflat"

if [ ! -f ./Fflat.exe ]; then
    cd ../ ;
    make Fflat.exe;
    cd bin;
fi

# test if computer has the right packages
python --version &> /dev/null || (echo "Could not find python 2" && exit 1)
python -c "import midiutil"   || (echo "you need python-midiutil to proceed" && exit 1)
aplaymidi --version &> /dev/null || (echo "Could not find aplaymidi" && exit 1)
grep audio /etc/group &> /dev/null || (echo "You need a soundcard to proceed" && exit 1)
fluidsynth --version &> /dev/null || (echo "install fluidsynth" && exit 1)
groups | grep -o audio &> /dev/null || (echo "add youself the the group 'audio'" && exit 1)

echo
echo "*************************************************************************"
echo "******************* Testing By Aural Inspection *************************"
echo "*************************************************************************"
echo "*If you cannot hear anything, remember to start fluidsynth at port 128:0*"
echo "*************************************************************************"
echo

# lille peter edderkop. - sound font should be /usr/share/sounds/sf2/FluidR3_GM.sf2
rm -f ../tests/test1.py ../tests/transposing.mid
./fflat -t 120 -o ../tests/test1.py -T transposing ../tests/ff_files/test1.fflat
cd ../tests/ ; python test1.py ; aplaymidi -p 128:0 transposing.mid; cd ../bin/ ;

# Something else for ../lib/sfx/Nice-Keys-B-JNv1.5.sf2
rm -f ../tests/test2.py ../tests/pianotest.mid
./fflat -t 120 -o ../tests/test2.py -T pianotest ../tests/ff_files/test2.fflat
# cd ../tests/ ; python test2.py ; aplaymidi -p 128:0 pianotest.mid; cd ../bin/ ;
