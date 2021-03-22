{ pkgs ? import <nixpkgs> { } }:

with pkgs;
mkShell {
  buildInputs = [ erlangR23 ];

  # Fix GLIBC Locale
  LOCALE_ARCHIVE = stdenv.lib.optionalString stdenv.isLinux
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
  LANG = "en_US.UTF-8";

  ERL_INCLUDE_PATH = "${erlangR23}/lib/erlang/usr/include";
}
