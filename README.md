# AdaCLIPS

## What is this? 

This repo contains Ada bindings for the [CLIPS rule-based programming language](http://clipsrules.net/) and is a fork of [Ted Dennison's AdaCLIPS work](https://web.archive.org/web/20151019121345/http://www.telepath.com/~dennison/Ted/AdaClips/AdaClips.html). It is unrelated to [the Open Channel Foundation's CLIPS/Ada](https://web.archive.org/web/20071213065206/http://www.openchannelfoundation.org/projects/CLIPS-ADA), an Ada implementation of CLIPS that costs $400.

## Setup

1. In order to build the example, you will need [`gprbuild`](https://github.com/AdaCore/gprbuild). You can also download the entire [GNAT Programming Studio](https://www.adacore.com/download/more), which includes `gprbuild`.

2. Copy `clips.ads` and `clips.adb` into the `example` directory.

3. Download the [latest stable CLIPS core sources](https://sourceforge.net/projects/clipsrules/files/CLIPS/6.31/) (`clips_core_source_631.tar.gz` or `clips_core_source_631.zip`) and extract them to a directory inside the `example` directory named `clips`.

4. Find the patch named `setup.h.patch` in the `allow_globals` directory and apply it to `setup.h` (part of the CLIPS core):
    ```shell
    <../../allow_globals/setup.h.patch patch setup.h;
    ```

5. In the `example` directory, run `gprbuild`. If it compiles successfully, you will have an executable named `hello_world`. Running it should generate the following output (from `hello_world.clp`):
    ```
    Hello world!
    ```