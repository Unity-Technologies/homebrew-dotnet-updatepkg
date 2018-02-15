class DotnetUpdatepkg < Formula
  homepage "https://github.com/Unity-Technologies/dotnet-updatepkg"
  head "https://github.com/Unity-Technologies/dotnet-updatepkg.git"

  def install
    bin.install 'dotnet-updatepackage', 'dotnet-updatepackage.fsx'
  end
end
