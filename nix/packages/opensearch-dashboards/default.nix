{
  lib,
  coreutils,
  fetchurl,
  makeWrapper,
  nodejs_22,
  stdenv,
  which,
}:

with lib;

stdenv.mkDerivation rec {
  pname = "opensearch-dashboards";
  # OpenSearch Dashboards refuses a server of another major version or an older
  # minor: move together with the server pin in opensearch/nixos-module.nix.
  version = "2.19.6";

  src = fetchurl {
    url = "https://artifacts.opensearch.org/releases/bundle/opensearch-dashboards/${version}/${pname}-${version}-linux-x64.tar.gz";
    hash = "sha256-8HWY0icxuy/fXeXX3eIBJLyDJqWuGHPMmYYE+vHSVMw=";
  };

  dontStrip = true;

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/libexec/opensearch-dashboards $out/bin
    mv * $out/libexec/opensearch-dashboards/
    rm -r $out/libexec/opensearch-dashboards/node
    # bin/use_node runs OSD_NODE_HOME's node. The bundled Node 18 left nixpkgs;
    # 22 is the newest line upstream OSD supports.
    for bin in $out/libexec/opensearch-dashboards/bin/opensearch-dashboards*; do
      makeWrapper $bin $out/bin/$(basename $bin) \
        --prefix PATH : "${
          lib.makeBinPath [
            nodejs_22
            coreutils
            which
          ]
        }" \
        --set OSD_NODE_HOME ${nodejs_22}
    done
    rm -rf $out/libexec/opensearch-dashboards/plugins/securityDashboards
  '';

  meta = {
    description = "Visualization and user interface for OpenSearch";
    homepage = "https://opensearch.org";
    license = licenses.asl20;
    platforms = with platforms; linux;
    mainProgram = "opensearch-dashboards";
  };
}
