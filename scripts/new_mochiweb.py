#!/usr/bin/env python
import os
import shutil

def copy_file(fn, dirpath, outpath, name):
    src = os.path.join(dirpath, fn)
    dst = os.path.join(outpath, fn.replace('skel'))
    
def spawn_skel(src, dst, name):
    for dirpath, dirnames, filenames in os.walk(src):
        dirnames[:] = [n for n in dirnames if not n.startswith('.')]
        outpath = dst + dirpath[len(src):].replace('skel', name)
        if not (os.path.exists(outpath) and
                os.path.abspath(outpath) == os.path.abspath(dst)):
            os.makedirs(outpath)
        print outpath + '/'
        for fn in filenames:
            if fn.startswith('.'):
                continue
            srcfn = os.path.join(dirpath, fn)
            dstfn = os.path.join(outpath, fn.replace('skel', name))
            data = file(srcfn, 'rb').read().replace('skel', name)
            file(dstfn, 'wb').write(data)
            shutil.copymode(srcfn, dstfn)
            print '  ' + dstfn

def main():
    import sys
    if len(sys.argv) == 2:
        dst = name = sys.argv[1]
    elif len(sys.argv) == 3:
        name, dst = sys.argv[1:3]
    else:
        raise SystemExit(sys.argv[0] + " name [destdir]")
    mochiweb = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    src = os.path.join(mochiweb, 'priv', 'skel')
    spawn_skel(src, dst, name)
    os.symlink(mochiweb, os.path.join(dst, 'deps', os.path.basename(mochiweb)))

if __name__ == '__main__':
    main()
