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
echo "********** - Remember to start fluidsynth at port 128:0 - ***************"
echo "*************************************************************************"

echo "********** - Running : Twinkle Twinkle **********************************"
echo "*************************************************************************"
rm -f ../tests/twinkle.py ../tests/twinkle.mid
./fflat -t 90 -o ../tests/twinkle.py -T twinkle ../tests/ff_files/twinkle.fflat
cd ../tests/ ; python twinkle.py ; aplaymidi -p 128:0 twinkle.mid; cd ../bin/ ;

echo "********** - Running : Example Piece ************************************"
echo "*************************************************************************"
rm -f ../tests/piece.py ../tests/piece.mid
./fflat -t 90 -o ../tests/piece.py -T piece ../tests/ff_files/piece.fflat
cd ../tests/ ; python piece.py ; aplaymidi -p 128:0 piece.mid; cd ../bin/ ;

echo "********** - Running : Scales *******************************************"
echo "*************************************************************************"
rm -f ../tests/scales.py ../tests/scales.mid
./fflat -t 90 -o ../tests/scales.py -T scales ../tests/ff_files/scales.fflat
cd ../tests/ ; python scales.py ; aplaymidi -p 128:0 scales.mid; cd ../bin/ ;

echo "********** - Running : Modes ********************************************"
echo "*************************************************************************"
rm -f ../tests/modes.py ../tests/modes.mid
./fflat -t 90 -o ../tests/modes.py -T modes ../tests/ff_files/modes.fflat
cd ../tests/ ; python modes.py ; aplaymidi -p 128:0 modes.mid; cd ../bin/ ;

echo
