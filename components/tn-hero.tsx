"use client"

import { Button } from "@/components/ui/button"
import { ArrowRight, Play } from "lucide-react"

export function TNHero() {
  return (
    <section className="relative min-h-screen flex items-center justify-center overflow-hidden pt-24 pb-16">
      {/* Background Effects */}
      <div className="absolute inset-0 overflow-hidden">
        {/* Radial gradient spotlight */}
        <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[800px] h-[800px] bg-primary/10 rounded-full blur-[120px] opacity-50" />
        <div className="absolute top-1/4 right-1/4 w-[400px] h-[400px] bg-primary/5 rounded-full blur-[80px] opacity-40" />
        
        {/* Subtle grid texture */}
        <div 
          className="absolute inset-0 opacity-[0.02]"
          style={{
            backgroundImage: `linear-gradient(rgba(255,255,255,0.1) 1px, transparent 1px), linear-gradient(90deg, rgba(255,255,255,0.1) 1px, transparent 1px)`,
            backgroundSize: '60px 60px'
          }}
        />
      </div>

      <div className="relative z-10 max-w-6xl mx-auto px-6 text-center">
        {/* Trust Badge */}
        <div className="animate-fade-in-badge">
          <span className="inline-flex items-center gap-2 px-4 py-2 rounded-full border border-border/50 bg-card/50 backdrop-blur-sm text-sm text-muted-foreground mb-8">
            <span className="w-2 h-2 rounded-full bg-primary animate-pulse" />
            Creative Agency for Modern Brands
          </span>
        </div>

        {/* Main Headline */}
        <h1 className="animate-fade-in-heading text-4xl sm:text-5xl md:text-6xl lg:text-7xl font-bold tracking-tight text-foreground mb-6 text-balance">
          Elevate Your Brand With{" "}
          <span className="text-primary">Creative</span> That Performs
        </h1>

        {/* Supporting Text */}
        <p className="animate-fade-in-subheading text-lg md:text-xl text-muted-foreground max-w-2xl mx-auto mb-10 text-pretty">
          TN Creative helps brands grow through social media management, content creation, 
          video production, branding, and website design.
        </p>

        {/* CTA Buttons */}
        <div className="animate-fade-in-buttons flex flex-col sm:flex-row items-center justify-center gap-4">
          <Button
            size="lg"
            className="rounded-full bg-primary hover:bg-primary/90 text-primary-foreground px-8 py-6 text-base font-medium group"
            asChild
          >
            <a href="tel:+27725742696">
              Get Started
              <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
            </a>
          </Button>
          <Button
            size="lg"
            variant="outline"
            className="rounded-full border-border/50 bg-card/50 backdrop-blur-sm hover:bg-card text-foreground px-8 py-6 text-base font-medium"
          >
            <Play className="mr-2 h-4 w-4" />
            View Our Work
          </Button>
        </div>

        {/* Stats Row */}
        <div className="animate-fade-in-trust mt-16 pt-8 border-t border-border/30 flex flex-wrap items-center justify-center gap-8 md:gap-16">
          <div className="text-center">
            <p className="text-3xl md:text-4xl font-bold text-foreground">50+</p>
            <p className="text-sm text-muted-foreground mt-1">Projects Delivered</p>
          </div>
          <div className="text-center">
            <p className="text-3xl md:text-4xl font-bold text-foreground">30+</p>
            <p className="text-sm text-muted-foreground mt-1">Brands Supported</p>
          </div>
          <div className="text-center">
            <p className="text-3xl md:text-4xl font-bold text-foreground">500+</p>
            <p className="text-sm text-muted-foreground mt-1">Content Produced</p>
          </div>
        </div>
      </div>
    </section>
  )
}
