DEST=/usr/local/bin/dotnet-updatepkg

rm -f $DEST

ln -s "$PWD/dotnet-updatepkg.sh" $DEST
chmod +x $DEST

