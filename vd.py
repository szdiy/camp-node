#!/usr/bin/env python

import httplib2, json, os, socket, time
from subprocess import Popen, call, PIPE

# FIXME: should allocate by server
nid = '1'
register_url = "http://localhost/scm/register/" + nid
check_url = "http://localhost/scm/check/" + nid
update_url = "http://localhost/scm/update/" + nid
download_url = "http://localhost/upload/"
VIDEO_PATH = 'video'

def get_json(url, method):
    h = httplib2.Http()
    while(True):
        try:
            (res, content) = h.request(url, method)
            return content
        except socket.error:
            print("Seems network error! Try it again...")
            time.sleep(5)
            continue
 
def check_newfile():
    j = json.loads(get_json(check_url, "GET"))
    if j['status'] == 'no':
        print("No new video")
        return False
    else: return True

def update_file(f):
    j = json.loads(get_json(update_url + '/' + f, "GET"))
    return j['status'] == 'ok'

def get_all_video(path):
    ret = []
    for root, dirs, files in os.walk(path, topdown=False):
        for name in files:
            ret.append(os.path.join(root, name))
    return ret

def comp_mtime(x, y):
    xm = os.stat(x).st_mtime
    ym = os.stat(y).st_mtime
    if xm > ym: return 1
    elif xm < ym:  return -1
    else: return 0

def detect_files_to_play():
    files = get_all_video(VIDEO_PATH)
    if len(files) == 0: return False
    files.sort(comp_mtime, reverse=True)
    print files;
    return files

def play_list(files):
    #process = Call("omxplayer --no-keys -o hdmi " + ' '.join(files), shell=True)
    if files:
        print("playing..." + ' '.join(files))
        call("find upload -type f > vd.plist", shell=True)
        return Popen("mplayer -playlist vd.plist", shell=True)
    else:
        return False

def download_file(filename):
    url = download_url + filename
    print(url)
    call("wget -c '" + url + "' -P video", shell=True)
    print("Downloaded %s" % filename)
    update_file(filename)
    print("Updated %s" % filename)
    
def update_newfile():
    j = json.loads(get_json(check_url, "GET"))
    if j['status'] == 'update':
        print("Files need to update: %s" % j['new-video'])
        for f in j['new-video']: download_file(f)
    else: raise Exception("BUG: wrong status, shouldn't be here if there's no new files!")

def main():
    get_json(register_url, "GET")
    while(True):
        print("check now...")
        p = play_list(detect_files_to_play())
        if check_newfile(): update_newfile()
        if p: p.wait()
        time.sleep(1)

if __name__ == "__main__":
    main()
