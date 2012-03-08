<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<xsl:import href="file:///c:/Applications/cygwin/home/xae-dev/xae/doctypes/docbook/styles/docbook/html/docbook.xsl" />

<xsl:variable name="chapter.autolabel">0</xsl:variable>
<xsl:variable name="generate.division.toc">0</xsl:variable>
<xsl:variable name="generate.component.toc">0</xsl:variable>

<xsl:template match="/">
  <xsl:variable name="doc" select="*[1]"/>
  <html>
  <head>
    <link rel="StyleSheet" href="../css/jde_style.css" TYPE="text/css"></link>
    <xsl:call-template name="head.content">
      <xsl:with-param name="node" select="$doc"/>
    </xsl:call-template>
    <xsl:call-template name="user.head.content"/>
  </head>
  <body xsl:use-attribute-sets="body.attrs">
    <xsl:apply-templates/>
  </body>
  </html>
</xsl:template>

<xsl:template match="ulink">
  <a>
    <xsl:if test="@id">
      <xsl:attribute name="name"><xsl:value-of select="@id"/></xsl:attribute>
    </xsl:if>
    <xsl:attribute name="href"><xsl:value-of select="@url"/></xsl:attribute>
    <xsl:if test="@type">
      <xsl:attribute name="target"><xsl:value-of select="@type"/></xsl:attribute>
    </xsl:if>
    <xsl:choose>
      <xsl:when test="string-length(.)=0">
	<xsl:value-of select="@url"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates/>
      </xsl:otherwise>
    </xsl:choose>
  </a>
</xsl:template>

</xsl:stylesheet>

