cd -releases

del dbadapters-bin-%1.exe
ren dbadapters-bin-$$$.exe dbadapters-bin-%1.exe"

del dbadapters-src-%1.exe
ren dbadapters-src-$$$.exe dbadapters-src-%1.exe"
cd ..
