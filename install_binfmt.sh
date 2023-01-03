#!/bin/sh
echo ":telda:M::álvur2\\n::$(realpath target/release/t):" | sudo tee /proc/sys/fs/binfmt_misc/register