class DotnetUpdatepkg < Formula
  homepage "https://gitlab.internal.unity3d.com/CDS/dotnet-updatepkg"
  head "https://gitlab.internal.unity3d.com/CDS/dotnet-updatepkg.git"

  def install
    bin.install 'dotnet-updatepackage', 'dotnet-updatepackage.fsx'
  end
end
