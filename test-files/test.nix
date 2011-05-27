{
  gitTests = {
    autoUpdateImpl = {
      # REGION AUTO UPDATE: { name="vim-addon-nix"; type="git"; url="git://github.com/MarcWeber/vim-addon-nix.git"; groups = "test"; }
      src = (fetchurl { url = "http://mawercer.de/~nix/reposvim-addon-nix-git-e204f.tar.bz2"; sha256 = "b6c0d6bb0f1c887e00f3817683ab7695b6011e9a9caad5034987445da365171a"; });
      name = "vim-addon-nix-git-e204f";
      # END
    };

    gemImpl = {

      # REGION GEM: { name="nixpkgs-ruby-overlay-gem-plugin"; type="git"; url="git://gitorious.org/nixpkgs-ruby-overlay/nixpkgs-ruby-overlay-gem-plugin.git"; groups = "test"; }
        "nixpgks-ruby-overlay-gem-plugin"."0.2" = {
          # spec.date: 2011-03-02 00:00:00 +0100
          name = "nixpgks-ruby-overlay-gem-plugin";
          version = "0.2";
          bump = "1";
          platform = "ruby";
          developmentDependencies = [  ];
          runtimeDependencies = [  ];
          dependencies =        [  ];
          src = (fetchurl { url = "http://mawercer.de/~nix/reposnixpkgs-ruby-overlay-gem-plugin-2.0-git-2366f.gem"; sha256 = "2a7f51cd137b0eca51f4a3e6376633ba1a51d182c5c5e89d585f7016d26e35d9"; });
          meta = {
            homepage = "http://gitorious.org/nixpkgs-ruby-overlay/nixpkgs-ruby-overlay-gem-plugin";
            license = []; # one of ?
            description = "Adds 'gem nixpkgsoverlay' command that dumps all gems into format readable by nix-pkgs-ruby-overlay";
          };
        };
      # END
    };
  };
}







