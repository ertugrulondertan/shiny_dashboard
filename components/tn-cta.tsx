"use client"

import { Button } from "@/components/ui/button"
import { ArrowRight, Mail } from "lucide-react"

export function TNCTA() {
  return (
    <section id="contact" className="py-24 md:py-32">
      <div className="max-w-4xl mx-auto px-6">
        <div className="relative rounded-3xl border border-border/50 bg-card/50 backdrop-blur-sm p-8 md:p-16 text-center overflow-hidden">
          {/* Background Effects */}
          <div className="absolute top-0 left-1/2 -translate-x-1/2 w-[500px] h-[300px] bg-primary/10 rounded-full blur-[100px] -z-10" />
          
          {/* Content */}
          <span className="inline-block px-4 py-1.5 rounded-full border border-border/50 bg-card/50 text-sm text-muted-foreground mb-6">
            Start Your Project
          </span>
          
          <h2 className="text-3xl md:text-4xl lg:text-5xl font-bold text-foreground mb-6 text-balance">
            Let's Build Something{" "}
            <span className="text-primary">Powerful</span> Together
          </h2>
          
          <p className="text-lg text-muted-foreground max-w-2xl mx-auto mb-10 text-pretty">
            Whether you need content, strategy, social media management, or a full digital presence, 
            TN Creative is ready to help your brand grow.
          </p>

          {/* CTA Buttons */}
          <div className="flex flex-col sm:flex-row items-center justify-center gap-4">
            <Button
              size="lg"
              className="rounded-full bg-primary hover:bg-primary/90 text-primary-foreground px-8 py-6 text-base font-medium group"
            >
              Book a Call
              <ArrowRight className="ml-2 h-4 w-4 group-hover:translate-x-1 transition-transform" />
            </Button>
            <Button
              size="lg"
              variant="outline"
              className="rounded-full border-border/50 bg-card/50 backdrop-blur-sm hover:bg-card text-foreground px-8 py-6 text-base font-medium"
            >
              <Mail className="mr-2 h-4 w-4" />
              Send an Enquiry
            </Button>
          </div>
        </div>
      </div>
    </section>
  )
}
