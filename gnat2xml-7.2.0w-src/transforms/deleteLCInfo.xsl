<?xml version="1.0" encoding="UTF-8" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!--
     This stylesheet removes the line and column attributes from AXF
     (Avatox Xml Format) documents in order to decrease visual clutter.
     Obviously if an application needs that information, this stylesheet
     should not be applied.

     Original Author:   Marc A. Criley
                        McKae Technologies
			mckae-support@mckae.com

     The content of this stylesheet is public domain.
-->

   <xsl:output method="xml"/>

   <!-- Identify transformation -->
   <xsl:template match="@*|node()">
      <xsl:copy>
         <xsl:apply-templates select="@*|node()"/>
      </xsl:copy>
   </xsl:template>

   <xsl:template match="@startLine"/>
   <xsl:template match="@startCol"/>
   <xsl:template match="@endLine"/>
   <xsl:template match="@endCol"/>

</xsl:stylesheet>

	 
