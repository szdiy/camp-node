#!/usr/bin/env python
# encoding=utf8

import httplib2, json, os, socket, time, signal, fcntl, struct
from subprocess import Popen, call, PIPE, check_output
from uuid import getnode as get_mac
import locale, re

import sys  
reload(sys)  
sys.setdefaultencoding('utf8')

def get_ip_address(ifname):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    return socket.inet_ntoa(fcntl.ioctl(
            s.fileno(),
            0x8915,  # SIOCGIFADDR
            struct.pack('256s', ifname[:15])
            )[20:24])

HEART_BEAT_PERIOD = 30 # 30 seconds
nid = get_mac() # MAC address as the node ID
register_url = "http://api.hack42.com/scm/node/register/%s" % nid
check_url = "http://api.hack42.com/scm/video/check/%s" % nid
update_url = "http://api.hack42.com/scm/video/update/%s" % nid
download_url = "http://api.hack42.com/upload/"
heartbeat_url = "http://api.hack42.com/scm/heartbeat/%s/%s/%s"
notice_url = "http://api.hack42.com/notice.jpg"
check_notice_url = "http://api.hack42.com/scm/notice/check/%s" % nid
clean_notice_url = "http://api.hack42.com/scm/notice/clean/%s" % nid
VIDEO_PATH = 'video'

def decode_json(j):
    try:
       ret = json.loads(j)
    except ValueError, e:
       print 'JSON parsing failed, possibly because Artanis is down!'
       return False
    return ret

def get_json(url, method):
    h = httplib2.Http(timeout=1)
    try:
        (res, content) = h.request(url, method)
        return decode_json(content)
    except socket.error:
           print("Seems network error! Try it again...")
           return False

def heartbeat(signum, frame):
    if signum == signal.SIGALRM:
        print "heart beat!"
        get_json(heartbeat_url % (nid, get_ip_address('eth0'), time.time()), "GET")
        signal.alarm(HEART_BEAT_PERIOD)

def check_newfile():
    j = get_json(check_url, "GET")
    if not j: return False
    if j['status'] == 'no':
        print("No new video")
        return False
    else: return True

def update_file(f):
    j = get_json(update_url + '/' + f, "GET")
    if not j: return False
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

def play_file(f):
    print("playing..." + f)
    p = Popen("omxplayer --win '0 0 1920 1080' --no-keys -o hdmi '%s'" % f, shell=True)
    #p = Popen("mplayer '%s'" % f, shell=True)
    if p: p.wait()

def download_file(filename):
    url = download_url + filename
    print(url)
    call("wget -b -c -o /dev/null '" + url + "' -P video", shell=True)
    print(u"Downloaded %s" % filename)
    update_file(filename)
    print(u"Updated %s" % filename)
    
def update_newfile():
    j = get_json(check_url, "GET")
    if not j: return False
    if j['status'] == 'update':
        print("Files need to update: %s" % j['new-video'])
        files = get_all_video(VIDEO_PATH)
        for f in j['new-video']:
            if VIDEO_PATH+f in files:
                print(f + u" no need to update")
                continue
            else: download_file(f)
    else: raise Exception("BUG: wrong status, shouldn't be here if there's no new files!")

def detect_notice_to_show():
    ret = False
    j = get_json(check_notice_url, "GET")
    if not j: return ret
    if j['status'] != 'no':
       ret = j
    print 'detect notice: %s' % ret
    return ret

def get_notice():
    call("wget -b -c -o /dev/null '%s'" % notice_url, shell=True)

def show_the_notice(notice):
    get_notice()
    print "There's notice for %r seconds" % notice['seconds']
    call("sudo fbi -a -T 1 -noverbose notice.jpg", shell=True)
    time.sleep(int(notice['seconds']))
    #p.kill()
    #get_json(clean_notice_url, "GET")
    call("rm -f notice.jpg", shell=True)
    call("cat /dev/zero > /dev/fb0", shell=True)

def try_notify():
    notice = detect_notice_to_show()
    if notice:
       show_the_notice(notice)

def main():
    while(True):
        print("check now...")
        try_notify()
        if check_newfile(): update_newfile()
        files = detect_files_to_play()
	if files and len(files) != 0:
           for f in files:
               play_file(f)
               try_notify()
        time.sleep(1)

def init():
    call("cat /dev/zero > /dev/fb0", shell=True)
    get_json(register_url, "GET")
    signal.signal(signal.SIGALRM, heartbeat)
    signal.alarm(1)

def check_start():
    ret = re.findall("vd\\.py", check_output(["ps", "aux"]))
    print ret
    if len(ret) > 1:
       print "vd.py is already started!"
       exit(0)
    return

if __name__ == "__main__":
    check_start()
    init()
    main()
