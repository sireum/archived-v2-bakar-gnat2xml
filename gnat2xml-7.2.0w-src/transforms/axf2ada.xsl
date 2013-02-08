<!DOCTYPE xsl:stylesheet [
<!ENTITY cr "<xsl:text>
</xsl:text>">
]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
	version="1.0">

	<!--
		Convert an AXF (Avatox XML Format) document into Ada source code.
		
		The compilability of the transformed code is strictly dependent on the
		content of the AXF text.
		
		Original Author:   Marc A. Criley
		McKae Technologies
		mckae-support@mckae.com
		
		Copyright (c) 2007, McKae Technologies
	-->

	<xsl:output method="text" />
	<xsl:strip-space elements="*" />

	<!-- Root element (codeRepresentation) transformation -->
	<xsl:template match="codeRepresentation">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="sourceLanguage">
		<xsl:text>-- Core language:  </xsl:text>
		<xsl:value-of select="@name" />&cr;
	</xsl:template>

	<!-- 
	    Compilation units
	-->

	<xsl:template match="aGenericPackage">
		<xsl:apply-templates />
	</xsl:template>

	<!-- Package spec -->
	<xsl:template match="aPackage">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="aPackageBody">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="aPackageInstance">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="aPackageRenaming">
		<xsl:apply-templates />
	</xsl:template>

	<xsl:template match="aProcedureBody">
		<xsl:apply-templates/>
	</xsl:template>
	
	<!--
	    Clauses
	-->
	
	<xsl:template match="aClause">
		<xsl:apply-templates select="*[1]"/>
		<xsl:text>;</xsl:text>&cr;
	</xsl:template>
	
	<xsl:template match="aRepresentationClause">
		<xsl:text>for </xsl:text>
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> use </xsl:text>
 	 	<xsl:apply-templates select="../anExpression[1]/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aUsePackageClause">
		<xsl:text>use </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aUseTypeClause">
		<xsl:text>use type </xsl:text>
		<xsl:apply-templates select="../anExpression[1]"/>
	</xsl:template>
	
	<xsl:template match="aWithClause">
		<xsl:text>with </xsl:text>
		<xsl:apply-templates select="../anExpression[1]"/>
	</xsl:template>
	
	<!--
	    Declarations
	-->
	
	<!--  General declaration matching template.  This template matches
	      most declarations-those that are terminated with a semi-colon,
	      but some, such as parameter specifications, are prefixed and
	      suffixed otherwise.  Those declarations are handled by more
	      specific templates (see which follow the general-purpose one).
	-->
	<xsl:template match="aDeclaration">
		<xsl:if test="current()=../aDeclaration[@visibility='internal'][1]">
			<xsl:text>private</xsl:text>&cr;
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
		<xsl:text>;</xsl:text>&cr;
	</xsl:template>

	<xsl:template match="aDeclaration[aDiscriminantSpecification]">
		<xsl:if test="not (preceding-sibling::aDeclaration)">
			<xsl:text>(</xsl:text>
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
		<xsl:choose>
			<xsl:when test="following-sibling::aDeclaration[1][aDiscriminantSpecification]">
				<xsl:text>;</xsl:text>&cr;
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="aDeclaration[aLoopParameterSpecification]">
		<xsl:apply-templates select="aLoopParameterSpecification"/>
	</xsl:template>

	<xsl:template match="aDeclaration[aChoiceParameterSpecification]">
		<xsl:apply-templates select="aDefiningName"/>
	</xsl:template>

	<xsl:template match="aDeclaration[anEnumerationLiteralSpecification]">
		<xsl:apply-templates select="aDefiningName"/>
		<xsl:if test="following-sibling::aDeclaration">
			<xsl:text>,</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template match="aDeclaration[aParameterSpecification]">
		<xsl:if test="not (preceding-sibling::aDeclaration[aParameterSpecification])">
			<xsl:text>(</xsl:text>
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
		<xsl:choose>
			<xsl:when test="following-sibling::aDeclaration[1][aParameterSpecification]">
				<xsl:text>;</xsl:text>&cr;
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="aComponentDeclaration">
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : </xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
		
		<!-- Optional default initialization -->
		<xsl:if test="../anExpression">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
		</xsl:if>
	</xsl:template>

	<!-- Constant declaration -->
	<xsl:template match="aConstantDeclaration">
		&cr;
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : constant </xsl:text>
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../aDefinition"/>
		
		<!-- Expression is omitted for a private constant -->
		<xsl:if test="../anExpression">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aDefaultInMode">
<!--  	<xsl:text> in </xsl:text>  -->
	</xsl:template>
	
	<!-- Deferred Constant declaration -->
	<xsl:template match="aDeferredConstantDeclaration">
		&cr;
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : constant </xsl:text>
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aDiscriminantSpecification">
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> : </xsl:text>
		<!-- This is the parameter type -->
		<xsl:apply-templates select="../anExpression[1]"/>
		
		<!-- This is the default value, if there is one -->
		<xsl:if test="../anExpression[2]">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>		
		</xsl:if>
	</xsl:template>

	<!-- Entry body declaration -->
	<xsl:template match="anEntryBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>entry </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> when </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />		
	</xsl:template>

	<xsl:template match="anEntryDeclaration">
		&cr;
		<xsl:text>entry </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
	</xsl:template>

	<xsl:template match="anExceptionDeclaration">
		<xsl:apply-templates select="following-sibling::*" />
		<xsl:text> : exception</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalFunctionDeclaration">
		&cr;
		<xsl:text>with function </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> return </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
	</xsl:template>

	<!-- Variable declaration -->
	<xsl:template match="aFormalObjectDeclaration">
		&cr;
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : </xsl:text>
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../anExpression[1]"/>
		
		<!-- Optional default initialization -->
		<xsl:if test="../anExpression[2]">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression[1]/following-sibling::*"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aFormalProcedureDeclaration">
		&cr;
		<xsl:text>with procedure </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
	</xsl:template>

	<xsl:template match="aFormalTypeDeclaration">
		<xsl:text>type </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is </xsl:text>
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
	</xsl:template>
	
	<!-- Function body declaration -->
	<xsl:template match="aFunctionBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>function </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> return </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />		
	</xsl:template>

	<xsl:template match="aFunctionDeclaration">
		&cr;
		<xsl:text>function </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> return </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:if test="anAbstractTrait">
			<xsl:text> is abstract</xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template match="aFunctionInstantiation">
		<xsl:text>function </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is new</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression"/>
		<xsl:apply-templates select="../anAssociation"/>
	</xsl:template>

	<!-- Generic function  declaration -->
	<xsl:template match="aGenericFunctionDeclaration">
		&cr;
		<xsl:text>generic</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/preceding-sibling::*
		                                    [not(self::aGenericFunctionDeclaration)]"/>
		&cr;
		<xsl:text>function </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> return </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
	</xsl:template>
	
	<!-- Generic package declaration -->
	<xsl:template match="aGenericPackageDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>generic</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/preceding-sibling::*
		                                    [not(self::aGenericPackageDeclaration)]"/>
		<xsl:text>package </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:for-each select="../aDefiningName/following-sibling::*">
			<xsl:if test="(@accessibility='internal')
						and not(preceding-sibling::*[@accessibility='internal'])">
				<xsl:text>private</xsl:text>&cr;
			</xsl:if>
			<xsl:apply-templates select="."/>
		</xsl:for-each>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<!-- Generic procedure declaration -->
	<xsl:template match="aGenericProcedureDeclaration">
		&cr;
		<xsl:text>generic</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/preceding-sibling::*
		                                    [not(self::aGenericProcedureDeclaration)]"/>
		&cr;
		<xsl:text>procedure </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
	</xsl:template>
	
	<xsl:template match="anIncompleteTypeDeclaration">
		<xsl:text>type </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anIntegerNumberDeclaration">
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> : constant := </xsl:text>
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aKnownDiscriminantPart">
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aLoopParameterSpecification">
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> in </xsl:text>
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../aDefinition"/>
	</xsl:template>
	
	<!-- Object Renaming declaration -->
	<xsl:template match="anObjectRenamingDeclaration">
		&cr;
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : </xsl:text>
		<xsl:apply-templates select="../anExpression[1]"/>
		
		<xsl:text> renames </xsl:text>
		<xsl:apply-templates select="../anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="anOrdinaryTypeDeclaration">
		<xsl:text>type </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is </xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
	</xsl:template>
	
	<!-- Package declaration -->
	<xsl:template match="aPackageDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:if test="../@accessibility = 'internal'">
			<xsl:text>private </xsl:text>
		</xsl:if>
		<xsl:text>package </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:for-each select="../aDefiningName/following-sibling::*">
			<xsl:if test="(@accessibility='internal')
						and not(preceding-sibling::*[@accessibility='internal'])">
				<xsl:text>private</xsl:text>&cr;
			</xsl:if>
			<xsl:apply-templates select="."/>
		</xsl:for-each>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<!-- Package body declaration -->
	<xsl:template match="aPackageBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>package body </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<xsl:template match="aPackageInstantiation">
		<xsl:text>package </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is new</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression"/>
		<xsl:apply-templates select="../anAssociation"/>
	</xsl:template>
	
	<!-- Package Renaming declaration -->
	<xsl:template match="aPackageRenamingDeclaration">
		&cr;
		<xsl:text>package </xsl:text>
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> renames </xsl:text>
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aParameterSpecification">
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> : </xsl:text>
		<xsl:choose>
			<xsl:when test="anInMode">
				<xsl:text> in </xsl:text>
			</xsl:when>
			<xsl:when test="anOutMode">
				<xsl:text> out </xsl:text>
			</xsl:when>
			<xsl:when test="anInOutMode">
				<xsl:text> in out </xsl:text>
			</xsl:when>
		</xsl:choose>
		<!-- This is the parameter type -->
		<xsl:apply-templates select="../anExpression[1]"/>
		
		<!-- This is the default value, if there is one -->
		<xsl:if test="../anExpression[2]">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>		
		</xsl:if>
	</xsl:template>

	<xsl:template match="aPrivateExtensionDeclaration">
		<xsl:text>type </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is new </xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
		<xsl:text> with private</xsl:text>
	</xsl:template>
	
	<xsl:template match="aPrivateTypeDeclaration">
		<xsl:text>type </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is </xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
	</xsl:template>
	
	<xsl:template match="aProcedureDeclaration">
		&cr;
		<xsl:text>procedure </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:if test="anAbstractTrait">
			<xsl:text> is abstract</xsl:text>
		</xsl:if>
	</xsl:template>

	
	<!-- Procedure body declaration -->
	<xsl:template match="aProcedureBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>procedure </xsl:text>
		<xsl:value-of select="$declName"/>
		<xsl:apply-templates select="../aDeclaration[aParameterSpecification]"/>
		<xsl:text> is</xsl:text>&cr;
		
		<!-- If there are procedure parameters, start the body processing following
		     them, if not, start processing everything after the procedure name. -->
		<xsl:choose>
			<xsl:when test="../aDeclaration[aParameterSpecification]">
				<xsl:apply-templates select="../aDeclaration[aParameterSpecification][last()]/following-sibling::*"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
			</xsl:otherwise>
		</xsl:choose>
		
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />		
	</xsl:template>
	
	<xsl:template match="aProcedureInstantiation">
		<xsl:text>procedure </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is new</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression"/>
		<xsl:apply-templates select="../anAssociation"/>
	</xsl:template>

	<!-- Procedure Renaming declaration -->
	<xsl:template match="aProcedureRenamingDeclaration">
		&cr;
		<xsl:text>procedure </xsl:text>
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:apply-templates select="../aDeclaration"/>
		<xsl:text> renames </xsl:text>
		<xsl:apply-templates select="../aDeclaration/following-sibling::*"/>
	</xsl:template>
	
	<!-- Protected body declaration -->
	<xsl:template match="aProtectedBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>protected body </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<xsl:template match="aRealNumberDeclaration">
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> : constant := </xsl:text>
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aSingleProtectedDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>protected </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<xsl:template match="aSingleTaskDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>task </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<xsl:template match="aSubtypeDeclaration">
		<xsl:text>subtype </xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
		<xsl:text> is </xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
	</xsl:template>
	
	<!-- Task body declaration -->
	<xsl:template match="aTaskBodyDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>task body </xsl:text>
		<xsl:value-of select="$declName"/>
		<xsl:text> is</xsl:text>&cr;
		
		<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
		
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />		
	</xsl:template>
	
	<!-- Task type declaration -->
	<xsl:template match="aTaskTypeDeclaration">
		<xsl:variable name="declName">
			<xsl:apply-templates select="../aDefiningName"/>
		</xsl:variable>
		&cr;
		<xsl:text>task type </xsl:text>
		<xsl:value-of select="$declName" />
		<xsl:choose>
			<xsl:when test="../aDefinition[aKnownDiscriminantPart]">
				<xsl:apply-templates select="../aDefinition[aKnownDiscriminantPart]"/>
				<xsl:text> is</xsl:text>&cr;
				<xsl:apply-templates select="../aDefinition[aKnownDiscriminantPart][last()]/following-sibling::*"/>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text> is</xsl:text>&cr;
				<xsl:apply-templates select="../aDefiningName/following-sibling::*"/>
			</xsl:otherwise>
		</xsl:choose>
		<xsl:text>end </xsl:text>
		<xsl:value-of select="$declName" />
	</xsl:template>
	
	<!-- Variable declaration -->
	<xsl:template match="aVariableDeclaration">
		&cr;
		<xsl:apply-templates select="../aDefiningName" />
		<xsl:text> : </xsl:text>
		<xsl:apply-templates select="*[1]"/>  <!-- Any trait -->
		<xsl:apply-templates select="../aDefinition"/>
		
		<!-- Optional default initialization -->
		<xsl:if test="../anExpression">
			<xsl:text> := </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
		</xsl:if>
	</xsl:template>
	
	<!--  Definitions -->
	<xsl:template match="aDefinition">
		<xsl:apply-templates select="*[1]"/>
	</xsl:template>
	
	<xsl:template match="anAccessToFunction">
		<xsl:text>access function</xsl:text>
		<xsl:apply-templates select="../../following-sibling::aDeclaration"/>
		&cr;
		<xsl:text>return </xsl:text>
		<xsl:apply-templates select="../../following-sibling::aDeclaration[last()]/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anAccessToProcedure">
		<xsl:text>access procedure</xsl:text>
		<xsl:apply-templates select="../../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anAccessToVariable">
		<xsl:text>access all </xsl:text>
		<xsl:apply-templates select="../../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anAccessTypeDefinition">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="aComponentDefinition">
		<xsl:apply-templates select="*[1]"/>	<!-- Any trait -->
		<xsl:apply-templates select="../*[not(self::aComponentDefinition)]"/>
	</xsl:template>
	
	<xsl:template match="aConstrainedArrayDefinition">
		<xsl:text>array (</xsl:text>
		<xsl:apply-templates select="../following-sibling::aDefinition[1]"/>
		<xsl:text>) of </xsl:text>
		<xsl:apply-templates select="../following-sibling::aDefinition[2]"/>
	</xsl:template>
	
	<xsl:template match="aConstraint">
		<xsl:apply-templates select="*[1]"/>
	</xsl:template>
	
	<xsl:template match="aDerivedRecordExtensionDefinition">
		<xsl:apply-templates select="*[1]"/>	<!-- Any trait -->
		<xsl:text>new </xsl:text>
		<xsl:apply-templates select="../following-sibling::aDefinition[1]"/>
		<xsl:text> with </xsl:text>
		<xsl:apply-templates select="../following-sibling::aDefinition[2]"/>
	</xsl:template>
	
	<xsl:template match="aDiscreteRange">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:if test="../anExpression[2]">
			<xsl:text> .. </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aDiscreteRangeAttributeReference">
		<!-- The relevant expression is up under the Definition node itself -->
		<xsl:apply-templates select="../../anExpression"/>
	</xsl:template>
	
	<xsl:template match="aDiscreteSimpleExpressionRange">
		<!-- The relevant expression is up under the Definition node itself -->
		<xsl:apply-templates select="../../anExpression[1]"/>
		<xsl:text> .. </xsl:text>
		<xsl:apply-templates select="../../anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="aDiscreteSubtypeDefinition">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="aDiscreteSubtypeIndication">
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aDiscriminantConstraint">
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anEnumerationTypeDefinition">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="../../*[not(self::aTypeDefinition)]"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalDiscreteTypeDefinition">
		<xsl:text>(&lt;&gt;)</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalModularTypeDefinition">
		<xsl:text>mod &lt;&gt;</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalFloatingPointDefinition">
		<xsl:text>digits &lt;&gt;</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalPrivateTypeDefinition">
		<xsl:apply-templates select="*[1]"/>   <!--  Any trait -->
	</xsl:template>
	
	<xsl:template match="aFormalSignedIntegerTypeDefinition">
		<xsl:text>range &lt;&gt;</xsl:text>
	</xsl:template>
	
	<xsl:template match="aFormalTypeDefinition">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="aFormalUnconstrainedArrayDefinition">
		<xsl:text>array (</xsl:text>
		<xsl:apply-templates select="../../anExpression"/>
		<xsl:text> range &lt;&gt;) of </xsl:text>
		<xsl:apply-templates select="../../anExpression/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anIndexConstraint">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="../following-sibling::*"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="aModularTypeDefinition">
		<xsl:text>mod </xsl:text>
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aNullRecordDefinition">
		<xsl:text>null record</xsl:text>
	</xsl:template>
	
	<xsl:template match="anOthersChoice">
		<xsl:text> others </xsl:text>
	</xsl:template>

	<xsl:template match="aPoolSpecificAccessToVariable">
		<xsl:text>access </xsl:text>
		<xsl:apply-templates select="../../following-sibling::*"/>
	</xsl:template>
		
	<xsl:template match="aPrivateExtensionDefinition">
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aPrivateTypeDefinition">
		<xsl:apply-templates select="*[1]"/>   <!--  Any trait -->
	</xsl:template>
	
	<xsl:template match="aProtectedDefinition">
		<xsl:for-each select="following-sibling::*">
			<xsl:if test="(@accessibility='internal')
						and not(preceding-sibling::*[@accessibility='internal'])">
				<xsl:text>private</xsl:text>&cr;
			</xsl:if>
			<xsl:apply-templates select="."/>
		</xsl:for-each>
	</xsl:template>
	
	<xsl:template match="aRecordDefinition">
		<xsl:text>record</xsl:text>&cr;
		<xsl:apply-templates select="../*[not(self::aRecordDefinition)]"/>
		<xsl:text>end record</xsl:text>
	</xsl:template>
	
	<xsl:template match="aRecordTypeDefinition">
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aSimpleExpressionRange">
		<!-- This test really bothers me.  The problem is that both definitions and
		and expressions (and who knows what else) will contain simple expression
		range constructs; but some of the constructs require the keyword "range",
		and some don't.  The distinguishing feature _appears_ to be whether it's
		embedded within an expression or something else (aDefinition), so that's
		what the test is here.  I'm not comfortable with it, but I'll stick with
		this until it breaks or I get a better handle on it. -->
		<xsl:if test="not(../../parent::node()[self::anExpression])">
	 		<xsl:text> range </xsl:text>
		</xsl:if>
		<xsl:apply-templates select="../following-sibling::anExpression[1]"/>
		<xsl:text> .. </xsl:text>
		<xsl:apply-templates select="../following-sibling::anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="aSignedIntegerTypeDefinition">
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aSubtypeIndication">
		<xsl:apply-templates select="../anExpression"/>
		<!-- If there's a constraint on the subtype indication, that'll be
		     a definition -->
		<xsl:apply-templates select="../aDefinition"/>
	</xsl:template>
	
	<xsl:template match="aTaggedRecordTypeDefinition">
		<xsl:apply-templates/>  <!-- Handles traits -->
		<xsl:if test="not(anAbstractLimitedTrait)">
			<xsl:text> tagged </xsl:text>
		</xsl:if>
		<xsl:apply-templates select="../following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aTypeDefinition">
		<xsl:apply-templates/>
	</xsl:template>
	
	<xsl:template match="aTaskDefinition">
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anUnconstrainedArrayDefinition">
		<xsl:text>array (</xsl:text>
		<xsl:apply-templates select="../following-sibling::anExpression"/>
		<xsl:text> range &lt;&gt;) of </xsl:text>
		<xsl:apply-templates select="../following-sibling::anExpression/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aDefiningExpandedName">
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text>.</xsl:text>
		<xsl:apply-templates select="../aDefiningName"/>
	</xsl:template>

	<xsl:template match="aDefiningEnumerationLiteral">
		<xsl:value-of select="@name"/>
	</xsl:template>

	<xsl:template match="aDefiningName">
		<xsl:apply-templates select="*[1]"/>
		<xsl:if test="following-sibling::aDefiningName">
			<xsl:text>, </xsl:text>
		</xsl:if>
	</xsl:template>

	<xsl:template match="aDefiningIdentifier">
		<xsl:value-of select="@name"/>
	</xsl:template>

	<xsl:template match="aDefiningOperatorSymbol">
		<xsl:text>"</xsl:text>
		<xsl:value-of select="@name"/>
		<xsl:text>"</xsl:text>
	</xsl:template>
	
	<!--
		Statements
	 -->
	
	<xsl:template match="aStatement">
		<xsl:if test="(parent::aDeclaration or preceding-sibling::aBlockStatement)
							 and not (preceding-sibling::aStatement
							 			or preceding-sibling::*[@isAStatement='true'])">
			<xsl:text>begin</xsl:text>&cr;
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
		<xsl:text>;</xsl:text>&cr;
	</xsl:template>
	
	<xsl:template match="anAbortStatement">
		<xsl:text>abort </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>

	<xsl:template match="anAcceptStatement">
		<xsl:variable name="entryName">
			<xsl:apply-templates select="../anExpression"/>
		</xsl:variable>
		<xsl:text>accept </xsl:text>
		<xsl:value-of select="$entryName" />
		<xsl:apply-templates select="../aDeclaration"/>
		<xsl:if test="../aStatement">
			<xsl:text>do</xsl:text>
			&cr;
			<xsl:apply-templates select="../aDeclaration[last()]/following-sibling::*"/>
			<xsl:text>end </xsl:text>
			<xsl:value-of select="$entryName" />		
		</xsl:if>
	</xsl:template>

	<xsl:template match="anAssignmentStatement">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> := </xsl:text>
		<xsl:apply-templates select="../anExpression[2]"/>
	</xsl:template>

	<xsl:template match="aBlockStatement">
		<xsl:variable name="stmtPos" 
		     select="count(following-sibling::*
		        [self::aStatement or @isAStatement][1]/preceding-sibling::*[not(self::aComment)]) + 1"/>
		<!-- The first thing within the block statement comes immediately after
		     the block statement element -->
		<xsl:if test="$stmtPos > 2">
			<xsl:text>declare</xsl:text>&cr;
			<xsl:apply-templates select="../*[(position() > 1) and (position() &lt; $stmtPos)]"/>
		</xsl:if>
 		<xsl:apply-templates select="../*[position() >= $stmtPos]"/>
		<xsl:text>end</xsl:text>
	</xsl:template>
	
	<xsl:template match="aCaseStatement">
		<xsl:text>case </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text> is</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression/following-sibling::*"/>
		<xsl:text>end case</xsl:text>
	</xsl:template>
	
	<xsl:template match="aConditionalEntryCallStatement">
		<xsl:apply-templates select="following-sibling::*"/>
		<xsl:text>end select</xsl:text>
	</xsl:template>
	
	<xsl:template match="aDelayRelativeStatement">
		<xsl:text>delay </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aDelayUntilStatement">
		<xsl:text>delay until </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anEntryCallStatement">
		<xsl:apply-templates select="../anExpression"/>
		<xsl:apply-templates select="../anExpression/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anExitStatement">
		<xsl:text>exit</xsl:text>
		<!-- Handle the label here -->
		<!-- 
		<xsl:if test="theresALabel">
			
		</xsl:if> -->
		<xsl:if test="../anExpression">
			<xsl:text> when </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aForLoopStatement">
		<xsl:text>for </xsl:text>
		<xsl:apply-templates select="../aDeclaration"/>
		<xsl:text> loop</xsl:text>&cr;
		<xsl:apply-templates select="../aDeclaration/following-sibling::*"/>
		<xsl:text>end loop</xsl:text>
	</xsl:template>

	<xsl:template match="anIfStatement">
		<xsl:apply-templates select="../aPath"/>
		<xsl:text>end if</xsl:text>
	</xsl:template>
	
		<xsl:template match="anIfPath">
			<xsl:text>if </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
			<xsl:text> then</xsl:text>&cr;
		</xsl:template>
	
		<xsl:template match="anElsifPath">
			<xsl:text>elsif </xsl:text>
			<xsl:apply-templates select="../anExpression"/>
			<xsl:text> then</xsl:text>&cr;
		</xsl:template>
	
		<xsl:template match="anElsePath">
			<xsl:text>else</xsl:text>&cr;
		</xsl:template>

	<xsl:template match="aNullStatement">
		<xsl:text>null</xsl:text>
	</xsl:template>

	<xsl:template match="aPath">
		<xsl:apply-templates select="*[1]"/>
		<!-- If and Elsif statements have an expression in the first position,
		     an Else statement of course does not -->
		<xsl:choose>
			<xsl:when test="aCasePath"/>
			<xsl:when test="aSelectPath"/>
			<xsl:when test="anElsePath or anOrPath[not(following-sibling::anExpression)]">
				<xsl:apply-templates select="*[position() > 1]"/>
			</xsl:when>
			<xsl:otherwise>  <!-- Elsif or an Or when -->
				<xsl:apply-templates select="*[position() > 2]"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="aCasePath">
		<xsl:choose>
			<xsl:when test="../anExpression">
				<xsl:text>when </xsl:text>
				<xsl:for-each select="../anExpression">
					<xsl:if test="position() > 1">
						<xsl:text> | </xsl:text>
					</xsl:if>
					<xsl:apply-templates select="."/>
				</xsl:for-each>
				<xsl:text> =></xsl:text>&cr;
				<xsl:apply-templates select="../anExpression[last()]/following-sibling::*"/>
			</xsl:when>
			<xsl:when test="../aDefinition/anOthersChoice">
				<xsl:text>when others =></xsl:text>&cr;
				<xsl:apply-templates select="../aDefinition/following-sibling::*"/>
			</xsl:when>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="aSelectPath">
		<xsl:text>select</xsl:text>&cr;
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anOrPath">
		<xsl:text>or </xsl:text>
		<xsl:choose>
			<xsl:when test="following-sibling::anExpression">
				<xsl:text>when </xsl:text>
				<xsl:apply-templates select="following-sibling::anExpression"/>
				<xsl:text> =></xsl:text>&cr;
				
			</xsl:when>
			<xsl:otherwise>
				&cr;
<!-- 			<xsl:apply-templates select="following-sibling::*"/>   -->			
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="aLoopStatement">
		<xsl:text> loop</xsl:text>&cr;
		<xsl:apply-templates select="following-sibling::*"/>
		<xsl:text>end loop</xsl:text>
	</xsl:template>

	<xsl:template match="aProcedureCallStatement">
		<xsl:choose>
			<xsl:when test="@prefixNotation='true'">
				<xsl:apply-templates select="../anAssociation[1]/anExpression"/>
				<xsl:text>.</xsl:text>
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation[position() > 1]">
					<xsl:with-param name="prefixedObject" select="true()"/>
				</xsl:apply-templates>
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="aRaiseStatement">
		<xsl:text>raise </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>

	<xsl:template match="aRequeueStatement">
		<xsl:text>requeue </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>

	<xsl:template match="aReturnStatement">
		<xsl:text>return </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>

	<xsl:template match="aSelectiveAcceptStatement">
		<xsl:apply-templates select="following-sibling::*"/>
		<xsl:text>end select</xsl:text>
	</xsl:template>

	<xsl:template match="aTerminateAlternativeStatement">
		<xsl:text>terminate</xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>  <!-- Comments, if any -->
	</xsl:template>

	<xsl:template match="aTimedEntryCallStatement">
		<xsl:apply-templates select="following-sibling::*"/>
		<xsl:text>end select</xsl:text>
	</xsl:template>

	<xsl:template match="aWhileLoopStatement">
		<xsl:text>while </xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text> loop</xsl:text>&cr;
		<xsl:apply-templates select="../aStatement"/>
		<xsl:text>end loop</xsl:text>
	</xsl:template>

	<!--
	    Expressions
	-->
	<xsl:template match="anExpression">
		<xsl:apply-templates select="*[1]"/>
	</xsl:template>
		
	<xsl:template match="anExpression/anAllocationFromQualifiedExpression">
		<xsl:text>new </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anExpression/anAllocationFromSubtype">
		<xsl:text>new </xsl:text>
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anExpression/anAndThenShortCircuit">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> and then</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="anAttributeReference">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text>'</xsl:text>
		<xsl:apply-templates select="../anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="anExpression/anExplicitDereference">
		<xsl:apply-templates select="../*[not(self::anExplicitDereference)]"/>
		<xsl:text>.all</xsl:text>
	</xsl:template>
	
	<xsl:template match="anExpression/aFunctionCall">
		<!-- Handle function calls differently, depending on whether
			 they're prefixed (i.e., a "regular" function call or an
			 infix operator. -->
		<xsl:choose>
			<xsl:when test="@prefixNotation='true'">
				<xsl:apply-templates select="../anAssociation[1]/anExpression"/>
				<xsl:text>.</xsl:text>
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation[position() > 1]">
					<xsl:with-param name="prefixedObject" select="true()"/>
				</xsl:apply-templates>
			</xsl:when>
			<xsl:when test="@prefixed='true'">
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation"/>
			</xsl:when>
			<xsl:when test="count(../anAssociation) = 1">
				<!-- Unary operator -->
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation"/>				
			</xsl:when>
			<xsl:otherwise>
				<xsl:apply-templates select="../anAssociation[1]" mode="isInfix"/>
				<xsl:apply-templates select="../anExpression"/>
				<xsl:apply-templates select="../anAssociation[position()>1]" mode="isInfix"/>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<xsl:template match="anExpression/anInRangeMembershipTest">
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text> in </xsl:text>
		<xsl:apply-templates select="../anExpression/following-sibling::*"/>
	</xsl:template>
		
	<xsl:template match="anExpression/anInTypeMembershipTest">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> in </xsl:text>
		<xsl:apply-templates select="../anExpression[1]/following-sibling::*"/>
	</xsl:template>
		
	<xsl:template match="anExpression/anOrElseShortCircuit">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> or else</xsl:text>&cr;
		<xsl:apply-templates select="../anExpression[1]/following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aNamedArrayAggregate">
		<xsl:apply-templates select="../*[not(self::aNamedArrayAggregate)]"/>
	</xsl:template>
	
	<xsl:template match="aParenthesizedExpression">
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="aPositionalArrayAggregate">
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="aQualifiedExpression">
		<xsl:variable name="needsParens" select="not(../anExpression[2]/aRecordAggregate)"/>
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text>'</xsl:text>
		<xsl:if test="$needsParens">
			<xsl:text>(</xsl:text>
		</xsl:if>
		<xsl:apply-templates select="../anExpression[2]"/>
		<xsl:if test="$needsParens">
			<xsl:text>)</xsl:text>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aRecordAggregate">
		<xsl:apply-templates select="following-sibling::*"/>
	</xsl:template>
	
	<xsl:template match="anExpression/anIndexedComponent">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text> (</xsl:text>
		<xsl:for-each select="../anExpression[position() > 1]">
			<xsl:apply-templates select="."/>
			<xsl:if test="following-sibling::anExpression">
				<xsl:text>, </xsl:text>
			</xsl:if>
		</xsl:for-each>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="anExpression/aSelectedComponent">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text>.</xsl:text>
		<xsl:apply-templates select="../anExpression[2]"/>
	</xsl:template>
	
	<xsl:template match="anExpression/aSlice">
		<xsl:apply-templates select="../anExpression"/>
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="../aDefinition"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="anExpression/aTypeConversion">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:text>(</xsl:text>
		<xsl:apply-templates select="../anExpression[2]"/>
		<xsl:text>)</xsl:text>
	</xsl:template>
	
	<xsl:template match="anAssociation">
		<xsl:param name="prefixedObject" select="false()"/>
		<xsl:if test="count(preceding-sibling::anAssociation) = 0
		                or ($prefixedObject and count(preceding-sibling::anAssociation) = 1)">
			<xsl:text>(</xsl:text>
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
		<xsl:choose>
			<xsl:when test="following-sibling::anAssociation">
				<xsl:text>, </xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>)</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
	
	<xsl:template match="anArrayComponentAssociation">
		<xsl:if test="../aDefinition">
			<xsl:apply-templates select="following-sibling::aDefinition"/>
			<xsl:text> => </xsl:text>
		</xsl:if>
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:if test="count(../anExpression) &gt; 1">
		
			<xsl:variable name="lastExpr" 
				select="../anExpression[last()]"/>
		
			<xsl:for-each
  		 select="../anExpression[1]/following-sibling::*
				[generate-id(following-sibling::*[1]) = generate-id($lastExpr)]">
				<xsl:if test="self::anExpression">
					<xsl:text> | </xsl:text>
				</xsl:if>
				<xsl:apply-templates select="."/>
			</xsl:for-each>
			
			<xsl:text> => </xsl:text>
			<xsl:apply-templates select="../anExpression[last()]"/>
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="anAssociation" mode="isInfix">
		<xsl:apply-templates select="*[1]"/>
	</xsl:template>
	
	<xsl:template match="aDiscriminantAssociation">
		<xsl:apply-templates select="../anExpression[1]"/>
		<!-- Check for named parameter passing-->
		<xsl:if test="../anExpression[2]">
			<xsl:text> => </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>			
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aGenericAssociation">
		<xsl:apply-templates select="../anExpression[1]"/>
		<!-- Check for named parameter passing-->
		<xsl:if test="../anExpression[2]">
			<xsl:text> => </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>			
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aPragmaArgumentAssociation">
		<xsl:apply-templates select="../anExpression"/>
	</xsl:template>
	
	<xsl:template match="aParameterAssociation">
		<xsl:apply-templates select="../anExpression[1]"/>
		<!-- Check for named parameter passing-->
		<xsl:if test="../anExpression[2]">
			<xsl:text> => </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>			
		</xsl:if>
	</xsl:template>
	
	<xsl:template match="aRecordComponentAssociation">
		<xsl:apply-templates select="../anExpression[1]"/>
		<xsl:if test="../anExpression[2]">
			<xsl:text> => </xsl:text>
			<xsl:apply-templates select="../anExpression[2]"/>
		</xsl:if>
	</xsl:template>
	
	<!-- Exceptions -->
	<xsl:template match="anExceptionHandler">
	<!--  This template skips all comments.  Rework later -->
		<xsl:if test="not(preceding-sibling::anExceptionHandler)">
			<xsl:text>exception</xsl:text>&cr;
		</xsl:if>
		<xsl:text>when </xsl:text>
		<xsl:choose>
			<xsl:when test="aDeclaration">
				<xsl:apply-templates select="aDeclaration/preceding-sibling::* 
												|aDeclaration"/>
				<xsl:text> : </xsl:text>
				<xsl:apply-templates select="aDeclaration[last()]/following-sibling::*
									[not(@isAStatement or self::aStatement or self::aComment)]"/>
			</xsl:when>
			<xsl:when test="anExpression">
				<xsl:apply-templates select="anExpression/preceding-sibling::*
												|anExpression"/>
				<xsl:apply-templates select="anExpression[last()]/following-sibling::*
									[not(@isAStatement or self::aStatement or self::aComment)]"/>
			</xsl:when>
			<xsl:when test="aDefinition">
				<xsl:apply-templates select="aDefinition/preceding-sibling::*
												|aDefinition"/>
				<xsl:apply-templates select="aDefinition[last()]/following-sibling::*
									[not(@isAStatement or self::aStatement or self::aComment)]"/>
			</xsl:when>
		</xsl:choose>
		
		<xsl:text> =></xsl:text>&cr;
		<xsl:apply-templates select="aStatement|*[@isAStatement]"/>
	</xsl:template>
	
	<xsl:template match="anExceptionHandler/anExpression">
		<!-- This specific template for expression handling takes care of the
		     alternation aspect of exception choices that are being caught. -->
		<xsl:if test="preceding-sibling::anExpression">
			<xsl:text> | </xsl:text>
		</xsl:if>
		<xsl:apply-templates select="*[1]"/>
	</xsl:template>
	
	<!-- Terminals -->
	<xsl:template match="aCharacterLiteral">
		<xsl:text>'</xsl:text>
		<xsl:value-of select="@literal"/>
		<xsl:text>'</xsl:text>
	</xsl:template>

	<xsl:template match="anIdentifier">
		<xsl:value-of select="@ident"/>
	</xsl:template>

	<xsl:template match="anIntegerLiteral">
		<xsl:value-of select="@literal"/>
	</xsl:template>

	<xsl:template match="anEnumerationLiteral">
		<xsl:value-of select="@literal"/>
	</xsl:template>
	
	<xsl:template match="aNullLiteral">
		<xsl:text>null</xsl:text>
	</xsl:template>

	<xsl:template match="aRealLiteral">
		<xsl:value-of select="@literal"/>
	</xsl:template>

	<xsl:template match="aStringLiteral">
		<xsl:text>"</xsl:text>
		<xsl:value-of select="@literal"/>
		<xsl:text>"</xsl:text>
	</xsl:template>

	<xsl:template match="anOperatorSymbol">
		<xsl:variable name="partOfFunction" select="../preceding-sibling::aFunctionCall"/>
		<xsl:choose>
			<xsl:when test="$partOfFunction">
				<xsl:text> </xsl:text>
				<xsl:value-of select="@operator"/>
				<xsl:text> </xsl:text>
			</xsl:when>
			<xsl:otherwise>
				<xsl:text>"</xsl:text>
				<xsl:value-of select="@operator"/>
				<xsl:text>"</xsl:text>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- Traits -->
	
	<xsl:template match="anAliasedTrait">
		<xsl:text>aliased </xsl:text>
	</xsl:template>
	
	<xsl:template match="anAbstractTrait">
		<xsl:text>abstract </xsl:text>
	</xsl:template>
	
	<xsl:template match="anAbstractLimitedTrait">
		<xsl:text>abstract tagged limited </xsl:text>
	</xsl:template>
	
	<!-- Ordinary trait has nothing associated with it -->
	<xsl:template match="anOrdinaryTrait"/>
	
	<xsl:template match="aPrivateTrait">
		<xsl:text>private</xsl:text>
	</xsl:template>
	
	<!-- Pragmas -->
	<xsl:template match="aPragma">
  	<xsl:if test="(@isAStatement='true')
  						and ((parent::aDeclaration or preceding-sibling::aBlockStatement)
							 and not (preceding-sibling::aStatement
							 			or preceding-sibling::*[@isAStatement='true']))">
			<xsl:text>begin</xsl:text>&cr;
		</xsl:if>
		<xsl:text>pragma </xsl:text>
		<xsl:apply-templates select="*[1]/@name"/>
		<xsl:apply-templates select="*[position() > 1]"/>
		<xsl:text>;</xsl:text>&cr;
	</xsl:template>
	
	<!-- Comment handling -->
	<xsl:template match="aComment/text()">
		<xsl:variable name="comText">
			<xsl:call-template name="trimEnds"/>
		</xsl:variable>

		<xsl:text>-</xsl:text>
		<xsl:value-of select="substring-after($comText, '-')"/>&cr;
	</xsl:template>

	<xsl:template match="aComment">
		<xsl:apply-templates select="text()"/>
	</xsl:template>

	<!--
   	   Suppressed element matches
	-->
 	<xsl:template match="pedigrees">
		<!-- Not matching anything with this yet -->
	</xsl:template>

	<xsl:template match="axfPoint">
		<!-- Not matching anything with this yet -->
	</xsl:template>

	<!--
	    Notification of elements that are Not Yet Handled
	-->
	<xsl:template match="*">
		&cr;
		<xsl:text>NYH: </xsl:text>
		<xsl:value-of select="name(.)"/>&cr;
	</xsl:template>


	<!-- 
	  Utilities 
	-->

	<!-- Utility template to trim the whitespace off the ends of strings -->
	<xsl:template name="trimEnds">
	
		<xsl:value-of select="name(.)"/>
	
		<xsl:variable name="before" select="preceding-sibling::node()" />
		<xsl:variable name="after" select="following-sibling::node()" />
		<xsl:variable name="conts" select="." />
		<xsl:variable name="contsl">
			<xsl:choose>
				<xsl:when test="count($before) = 0">
					<xsl:call-template name="remove-lf-left">
						<xsl:with-param name="astr" select="$conts" />
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="$conts" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>

		<xsl:variable name="contslr">
			<xsl:choose>
				<xsl:when test="count($after) = 0">
					<xsl:call-template name="remove-ws-right">
						<xsl:with-param name="astr" select="$contsl" />
					</xsl:call-template>
				</xsl:when>
				<xsl:otherwise>
					<xsl:value-of select="$contsl" />
				</xsl:otherwise>
			</xsl:choose>
		</xsl:variable>
		
		<xsl:value-of select="$contslr" />

	</xsl:template>


	<!-- eats linefeeds from the left -->
	<xsl:template name="remove-lf-left">
		<xsl:param name="astr" />

		<xsl:choose>
			<xsl:when
				test="starts-with($astr,'&#xA;') or
                    starts-with($astr,'&#xD;')">
				<xsl:call-template name="remove-lf-left">
					<xsl:with-param name="astr"
						select="substring($astr, 2)" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$astr" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

	<!-- eats whitespace from the right -->
	<xsl:template name="remove-ws-right">
		<xsl:param name="astr" />

		<xsl:variable name="last-char">
			<xsl:value-of
				select="substring($astr, string-length($astr), 1)" />
		</xsl:variable>

		<xsl:choose>
			<xsl:when
				test="($last-char = '&#xA;') or
                    ($last-char = '&#xD;') or
                    ($last-char = '&#x20;') or
                    ($last-char = '&#x9;')">
				<xsl:call-template name="remove-ws-right">
					<xsl:with-param name="astr"
						select="substring($astr, 1, string-length($astr) - 1)" />
				</xsl:call-template>
			</xsl:when>
			<xsl:otherwise>
				<xsl:value-of select="$astr" />
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>

</xsl:stylesheet>


