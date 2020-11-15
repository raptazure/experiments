import { prop, modelOptions } from '@typegoose/typegoose';
import { ApiProperty } from '@nestjs/swagger';

@modelOptions({
  schemaOptions: {
    timestamps: true,
  },
})
export class User {
  @ApiProperty({ description: 'Username', example: 'user1' })
  @prop()
  username: string;

  @ApiProperty({ description: 'Password', example: 'code1' })
  @prop()
  password: string;
}
