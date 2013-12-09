;; -*- coding: utf-8 -*-

(in-package :openrdf.model)

;; This is ugly, maybe there's some way to sidestep this with `eval-when'
(defun voc (symbol)
  (symbol-value (find-symbol symbol :openrdf.vocabulary)))

(defun datatype-from-python (value datatype)
  ;; this translation is all messed up, but will do for the time being
  ;; some date-related noise has been dropped, I don't understand how it
  ;; can be relevant.
  (typecase value
    (string (values value datatype))
    (boolean (values (if value "true" "false") (voc '+boolean+)))
    (bignum (values (write-to-string value) (voc '+long+)))
    (fixnum (values (write-to-string value) (voc '+integer+)))
    (float (values (write-to-string value) (voc '+double+)))
    (timestamp (values (format-timestring nil value) (voc '+datetime+)))
    (otherwise (values (write-to-string value) datatype))))

(defclass literal (value)
  ((label :initarg :label :accessor literal-label)
   (datatype :initform nil :initarg :datatype :accessor literal-datatype)
   (language :initform nil :initarg :language :accessor literal-language)))

(defmethod (setf literal-datatype) (value (this literal))
  (setf (slot-value this 'datatype) (ensure-uri value)))

(defmethod value= ((this literal) (that literal))
  (with-slots ((label-a label) (datatype-a datatype) (language-a language)) this
    (with-slots ((label-b label) (datatype-b datatype) (language-b language)) this
      (and (eqal label-a label-b)
           (eqal datatype-a datatype-b)
           (eqal language-a language-b)))))

(defmethod int-value ((this literal))
  (truncate (coerce (literal-label this) 'integer) #xffffffff))

(defmethod long-value ((this literal))
  (truncate (coerce (literal-label this) 'integer) #xffffffffffffffff))

(defmethod float-value ((this literal))
  (coerce (literal-label this) 'float))

(defmethod boolean-value ((this literal))
  (coerce (literal-label this) 'boolean))

(defmethod date-value ((this literal))
  (parse-timestring (coerce (literal-label this) 'string)))

(defmethod ntriples ((this literal))
  (format nil "\"~a\"~@[@~a~]~@[~@*^^~a~]"
          (openrdf.utils:encode-ntriple-string (literal-label this))
          (literal-language this)
          (ntriples (literal-datatype this))))

;; no idea why, this should be the choice of the compound-literal...
(defparameter +range-literal+ "rangeLiteral")

(defclass compound-literal (literal)
  ((choice :initarg :choice :accessor compound-literal-choice :type string)
   (lower-bound :initform nil :initarg :lower-bound :type literal-impl
                :accessor compound-literal-lower-bound)
   (upper-bound :initform nil :initarg :upper-bound :type literal-impl
                :accessor compound-literal-upper-bound)))

(defmethod range-literal-p ((this compound-literal))
  (string= (compound-literal-choice this) +range-literal+))

;; The original code extends this from compound-literal, but it doesn
;; make sense to do so...
(defclass range-literal (literal)
  ((lower-bound :initform nil :initarg :lower-bound :type literal-impl
                :accessor range-literal-lower-bound)
   (upper-bound :initform nil :initarg :upper-bound :type literal-impl
                :accessor range-literal-upper-bound)))

(defun geounitp (value) (member value '(:km :mile :radian :degree)))

(deftype geounit () '(satisfies geounitp))

(defclass geo-coordinate (compound-literal)
  ((x :initarg :x :accessor geo-coordinate-x)
   (y :initarg :y :accessor geo-coordinate-y)
   (unit :initform nil :initarg :unit :type geounit
                :accessor geo-coordinate-unit)
   (geo-type :initform nil :initarg :geo-type
                :accessor geo-coordinate-type)))

(defmethod print-object ((this geo-coordinate) stream)
  (with-slots (x y) this (format stream  "|COOR|(~d, ~d)" % x y)))

(defclass geo-spatial-region (compound-literal) ())

;; unit and geo-type are duplicated from `geo-coordinate' wtf?
(defclass geo-box (geo-spatial-region)
  ((x-min :initarg :x-min :accessor geo-spatial-region-x-min)
   (x-max :initarg :x-max :accessor geo-spatial-region-x-max)
   (y-max :initarg :y-max :accessor geo-spatial-region-y-max)
   (y-min :initarg :y-min :accessor geo-spatial-region-y-min)
   (unit :initform nil :initarg :unit :type geounit
                :accessor geo-coordinate-unit)
   (geo-type :initform nil :initarg :geo-type
                :accessor geo-coordinate-type)))

#|
    
###############################################################################
## Automatic conversion from Literal to Python object
###############################################################################
    def toPython(self):
        """
        Return a Python object representation of this literal.   
        Slightly silly implementation because we implement a conversion table
        and then don't use the conversion functions.     
        """
        return XSDToPython[getattr(self.datatype, "uri", None)](self)


XSDToPython = defaultdict(lambda: Literal.getValue, [
                (XMLSchema.INT.uri, Literal.intValue),
                (XMLSchema.FLOAT.uri, Literal.floatValue), 
                (XMLSchema.DOUBLE.uri, Literal.floatValue), 
                (XMLSchema.LONG.uri, Literal.intValue),
                (XMLSchema.INTEGER.uri, Literal.longValue),
                (XMLSchema.BOOLEAN.uri, Literal.booleanValue),
                (XMLSchema.DATETIME.uri, Literal.datetimeValue),
                (XMLSchema.DATE.uri, Literal.dateValue),
                (XMLSchema.TIME.uri, Literal.timeValue)])


###############################################################################
# Extension to Sesame API
###############################################################################

class CompoundLiteral(Literal):
    
class RangeLiteral(CompoundLiteral):

class GeoCoordinate(CompoundLiteral):
    """
    Define either a cartesian coordinate or a spherical coordinate.  For the
    latter, nit can be 'km', 'mile', 'radian', or 'degree'
    """
    def __init__(self, x, y, unit=None, geoType=None):
        self.xcoor = x
        self.ycoor = y
        self.unit = unit
        self.geoType = geoType
    
    def __str__(self):
        return "|COOR|(%i, %i)" % (self.xcoor, self.ycoor)
    
class GeoSpatialRegion(CompoundLiteral):
    pass

class GeoBox(GeoSpatialRegion):
    def __init__(self, xMin, xMax, yMin, yMax, unit=None, geoType=None):
        self.xMin = xMin
        self.xMax = xMax
        self.yMin = yMin
        self.yMax = yMax
        self.unit = unit
        self.geoType = geoType
    
    def __str__(self): return "|Box|%s,%s %s,%s" % (self.xMin, self.xMax, self.yMin, self.yMax)
        
class GeoCircle(GeoSpatialRegion):
    def __init__(self, x, y, radius, unit=None, geoType=None):
        self.x = x
        self.y = y
        self.radius = radius
        self.unit = unit
        self.geoType=geoType
        
    def __str__(self): return "|Circle|%i,%i, radius=%i" % (self.x, self.y, self.radius)

class GeoPolygon(GeoSpatialRegion):
    def __init__(self, vertices, uri=None, geoType=None):
        self.vertices = vertices
        self.geoType = geoType
        self.uri = uri
        self.resource = None
        self.miniPolygon = None
        
    def getVertices(self): return self.vertices
    
    def getResource(self): return self.resource
    
    def __str__(self): return "|Polygon|%s" % self.vertices
    
# The code below this line is modifed from the fixed_datetime.py file
# which can be found here:
#
# http://blog.twinapex.fi/2008/06/30/relativity-of-time-shortcomings-in-python-datetime-and-workaround/
#
# We don't wish to be dependant on pytz or monkeypatch datetime, so only
# portions were used.

# Copyright (c) 2008, Red Innovation Ltd., Finland
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * Neither the name of Red Innovation nor the names of its contributors 
#       may be used to endorse or promote products derived from this software 
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY RED INNOVATION ``AS IS'' AND ANY
# EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL RED INNOVATION BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

def _parse_iso(timestamp):
    """Parses the given ISO 8601 compatible timestamp string 
    and converts it to fixed_datetime.datetime. The timestamp
    must conform to following formats:

         - the format is DATE SEP TIME TIMEZONE without
           any intervening spaces.

         - the date must be in format YYYY-MM-DD

         - the time may be either
             * HH:MM:SS,FFFF
             * HH:MM,FFFF
             * HH,FFFF
           FFFF is the fractional part. Decimal point can be
           used too.

         - the time zone must be either Z, -HH:MM or +HH:MM

         - the date and time must be separated either by
           whitespace or single T letter

         - the separators - and : may all be omitted, or
           must all be present.

         Examples (Unix Epoch):

             1970-01-01T00:00:00Z 
             1970-01-01T00Z 
             1969-12-31 19,5-04:30
             19700101T030000+0300
    """
    timestamp = timestamp.strip()
    
    m = _parse_iso.parser.match(timestamp)
    if not m:
        raise ValueError("%s: Not a proper ISO 8601 timestamp!" % timestamp)

    year  = int(m.group('year'))
    month = int(m.group('month'))
    day   = int(m.group('day'))
    
    h, min, s, us = None, None, None, 0
    frac = 0
    if m.group('tzempty') == None and m.group('tzh') == None:
        raise ValueError("Not a proper ISO 8601 timestamp: " +
                "missing timezone (Z or +hh[:mm])!")

    if m.group('frac'):
        frac = m.group('frac')
        power = len(frac)
        frac  = long(frac) / 10.0 ** power

    if m.group('hour'):
        h = int(m.group('hour'))

    if m.group('minute'):
        min = int(m.group('minute'))

    if m.group('second'):
        s = int(m.group('second'))

    if frac != None:
        # ok, fractions of hour?
        if min == None:
           frac, min = _math.modf(frac * 60.0)
           min = int(min)

        # fractions of second?
        if s == None:
           frac, s = _math.modf(frac * 60.0)
           s = int(s)

        # and extract microseconds...
        us = int(frac * 1000000)

    if m.group('tzempty') == 'Z':
        offsetmins = 0
    else:
        # timezone: hour diff with sign
        offsetmins = int(m.group('tzh')) * 60
        tzm = m.group('tzm')
      
        # add optional minutes
        if tzm != None:
            tzm = long(tzm)
            offsetmins += tzm if offsetmins > 0 else -tzm

    # For our use here, we should not be given non-zero offsets
    assert offsetmins == 0

    return datetime.datetime(year, month, day, h, min, s, us)

import re

_parse_iso.parser = re.compile("""
    ^
    (?P<year> [0-9]{4})(?P<ymdsep>-?)
    (?P<month>[0-9]{2})(?P=ymdsep)
    (?P<day>  [0-9]{2})

    (?: # time part... optional... at least hour must be specified
	(?:T|\s+)
        (?P<hour>[0-9]{2})
        (?:
            # minutes, separated with :, or none, from hours
            (?P<hmssep>[:]?)
            (?P<minute>[0-9]{2})
            (?:
                # same for seconds, separated with :, or none, from hours
                (?P=hmssep)
                (?P<second>[0-9]{2})
            )?
        )?
        
        # fractions
        (?: [,.] (?P<frac>[0-9]{1,10}))?

        # timezone, Z, +-hh or +-hh:?mm. MUST BE, but complain if not there.
        (
            (?P<tzempty>Z) 
        | 
            (?P<tzh>[+-][0-9]{2}) 
            (?: :? # optional separator 
                (?P<tzm>[0-9]{2})
            )?
        )?
    )?
    $
""", re.X) # """
|#
