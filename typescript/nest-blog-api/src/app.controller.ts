import { Controller, Get } from '@nestjs/common';
import { ApiTags } from '@nestjs/swagger';

@Controller()
@ApiTags('default')
export class AppController {
  @Get()
  index(): string {
    return 'index';
  }
}
