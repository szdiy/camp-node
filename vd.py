#!/usr/bin/env python

import httplib2, json, os, socket, time, signal, fcntl, struct
from subprocess import Popen, call, PIPE
from uuid import getnode as get_mac

def get_ip_address(ifname):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    return socket.inet_ntoa(fcntl.ioctl(
            s.fileno(),
            0x8915,  # SIOCGIFADDR
            struct.pack('256s', ifname[:15])
            )[20:24])

HEART_BEAT_PERIOD = 30 # 30 seconds
nid = get_mac() # MAC address as the node ID
nip = get_ip_address('wlan0')
register_url = "http://localhost/scm/register/%s" % nid
check_url = "http://localhost/scm/check/%s" % nid
update_url = "http://localhost/scm/update/%s" % nid
download_url = "http://localhost/upload/"
heartbeat_url = "http://localhost/scm/heartbeat/%s/%s/%s"
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

def heartbeat(signum, frame):
    if signum == signal.SIGALRM:
        print "heart beat!"
        get_json(heartbeat_url % (nid, get_ip_address('wlan0'), time.time()), "GET")
        signal.alarm(HEART_BEAT_PERIOD)

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
        files = get_all_video(VIDEO_PATH)
        for f in j['new-video']:
            if 'video/'+f in files:
                print(f + " no need to update")
                continue
            else: download_file(f)
    else: raise Exception("BUG: wrong status, shouldn't be here if there's no new files!")

def main():
    get_json(register_url, "GET")
    while(True):
        print("check now...")
        p = play_list(detect_files_to_play())
        if check_newfile(): update_newfile()
        if p: p.wait()
        time.sleep(1)

def init():
    signal.signal(signal.SIGALRM, heartbeat)
    signal.alarm(1)
    # TODO: upload IP address

if __name__ == "__main__":
    init()
    main()
