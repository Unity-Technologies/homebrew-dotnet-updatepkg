class DotnetUpdatepkg < Formula
  homepage "https://gitlab.internal.unity3d.com/CDS/dotnet-updatepkg"
  head "git@gitlab.internal.unity3d.com:CDS/dotnet-updatepkg.git"

  def install
    bin.install 'dotnet-updatepackage', 'dotnet-updatepackage.fsx'
  end
end
