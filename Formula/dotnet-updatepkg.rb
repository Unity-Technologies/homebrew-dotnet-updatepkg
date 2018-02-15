class DotnetUpdatepkg < Formula
  script_version = "v1.1"

  desc "Utility script that can consolidate and update nuget packages across projects"
  
  homepage "https://github.com/Unity-Technologies/dotnet-updatepkg"
  url "https://github.com/Unity-Technologies/dotnet-updatepkg.git", :tag => script_version 
  head "https://github.com/Unity-Technologies/dotnet-updatepkg.git"

  def install
    bin.install 'dotnet-updatepkg', 'dotnet-updatepkg.fsx'
  end
end
