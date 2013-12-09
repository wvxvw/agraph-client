#|
def hex2int(hex):
    return int(hex, 16)

def int2hex(n):
    return "%X" % n

def ord2HHHH(n):
    digits = int2hex(n)
    length = min(len(digits), 8)
    return ord2HHHH.prefixes[length] + digits

ord2HHHH.prefixes = ['\U', '\u000', '\u00', '\u0', '\u', '\U000', '\U00', '\U0', '\U']

def encode_ntriple_string(string):
    """
    Return a unicode string encoded in 7-bit ASCII containing the
    NTRIPLES escape sequences for non-ascii and other characters.
    """

    # Access these at local variable speeds since they are in a loop
    HEX_MAP = encode_ntriple_string.HEX_MAP
    LOWER_ASCII = encode_ntriple_string.LOWER_ASCII
    QUOTE = encode_ntriple_string.QUOTE
    UPPER_ASCII = encode_ntriple_string.UPPER_ASCII
    
    bytes = []
    if not isinstance(string, unicode):
        string = unicode(string)

    for c in string:
        ordl = ord(c)
        if ordl >= LOWER_ASCII and ordl <= UPPER_ASCII and not ordl == QUOTE:
            bytes.append(c)
        else:
            bytes.append(HEX_MAP.get(ordl) or ord2HHHH(ordl))
    return ''.join(bytes)

encode_ntriple_string.HEX_MAP = {
    hex2int('9'): r'\t',
    hex2int('A'): r'\n',
    hex2int('D'): r'\r',
    hex2int('20'): chr(hex2int('20')), #blank
    hex2int('21'): chr(hex2int('21')),
    hex2int('22'): r'\"',    
    hex2int('5C'): r'\\',    
    }

encode_ntriple_string.LOWER_ASCII = hex2int('23')
encode_ntriple_string.QUOTE = hex2int('5C')
encode_ntriple_string.UPPER_ASCII = hex2int('7E')

def uriref(string): 
  uri = None
  if string[0] == '<': 
    match = uriref.pattern.match(string)
    assert match, "%s is not a valid URI." % string
    uri = match.group(1).decode('unicode-escape')
  return uri

uri_pattern = r'<([^:]+:[^\s"<>]+)>'
uriref.pattern = re.compile(uri_pattern + '$')

def nodeid(string): 
  bnode = None
  if string[0] == '_': 
     bnode = nodeid.pattern.match(string).group(1)
  return bnode

nodeid.pattern = re.compile(r'_:([A-Za-z][A-Za-z0-9]*)$')

def literal(string): 
  lit = None
  if string[0] == '"': 
     label, lang, dtype = literal.pattern.match(string).groups()
     lit = (label.decode('unicode-escape'), dtype, lang)
  return lit

litvalue = r'"([^"\\]*(?:\\.[^"\\]*)*)"'
litinfo = r'(?:@([a-z]+(?:-[a-z0-9]+)*)|\^\^' + uri_pattern + r')?'
literal.pattern = re.compile(litvalue + litinfo + '$')





def getLocalNameIndex(uri):
    idx = uri.rfind('#')
    if (idx < 0):
        idx = uri.rfind('/')
    if (idx < 0):
        idx = uri.rfind(':')
    if (idx < 0):
        raise IllegalArgumentException("No separator character found in URI: " + uri)
    return idx + 1

def asURIString(value):
    value = str(value)
    if value.startswith('<'): return value
    else: return "<%s>" % value
|#

(in-package :openrdf.utils)

(defun char->unicode-codepoint (char)
  (let ((hex (format nil "~x" (char-code char))))
    (replace (copy-seq "\\u0000") hex :start1 (- 6 (length hex)))))

(defun encode-ntriple-string (string)
  (with-output-to-string (stream)
    (iter
     (for c :in-string string)
     (princ 
      (case c
        (#\Tab "\\t")
        (#\Rubout "\\r")
        (#\Newline "\\n")
        (#\Space " ")
        (#\" "\\\"")
        (#\\ "\\")
        (otherwise
         (if (and (char>= c #\#)
                  (char<= c #\~)
                  (not (char= c #\')))
             (make-string 1 :initial-element c)
             (char->unicode-codepoint c)))) stream))))

(defun local-name-index (uri)
  (iter
    (initially (setq index -1))
    (for needle :in '(#\# #\/ #\:))
    (for index :next (when (< index 0) (position needle uri)))
    (finally
     (if (< index 0)
         (error "No separator found in URI '%s'" uri)
         (return (1+ index))))))

(defun ensure-uri-string (value)
  (if (char= (char value 0) #\<) value (format nil "<~a>" value)))

(defun local-name (uri)
  (subseq uri (local-name-index uri)))

(defun namespace (uri)
  (subseq uri 0 (local-name-index uri)))
